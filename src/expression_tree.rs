/*!
Both parsing and translation use a flat storage for their trees. This functionality
 is shared between them and so is extracted to this module. The documentation
 here reflects the parser's use of this module, but the translation module uses
 it in a very similar way.
*/

/// We could use [usize] and support truly massive trees, but that's simply not
///  practical. Four billion expressions should be enough for anyone and using
///  a smaller type as our index saves significant space in the expression
///  vector (since the largest members of the Expression enum have multiple
///  child ids) and less space taken means more efficient cache usage.
type IndexImpl = u32;

/// A newtype to wrap our index type that is smaller than usize and make it
///  convenient to go back and forth.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Index(pub IndexImpl);

impl From<Index> for usize {
    fn from(value: Index) -> Self {
        value.0 as usize
    }
}

impl From<usize> for Index {
    fn from(value: usize) -> Self {
        // panicking is never ideal, but also don't want to overflow and given
        //  our assumption that IndexImpl is big enough for any reasonable input,
        //  it's acceptable to panic.
        Self(u32::try_from(value).expect("index is too large to convert to IndexImpl type"))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExpressionId(pub Index);

impl From<ExpressionId> for usize {
    fn from(value: ExpressionId) -> Self {
        value.0.into()
    }
}

impl From<usize> for ExpressionId {
    fn from(value: usize) -> Self {
        Self(Index::from(value))
    }
}

/// Represents the arguments to a function (or sequence operation) by storing
///  an index and a length. Slicing [ExpressionTree.arg_lists] using these
///  will results in the [ExpressionId]s of the arguments.
///
/// Note: when parsing a list we can utilize a single scratch Vec like so:
///   stack frame 1:
///     parse call expression
///     loop and recurse to parse arguments
///     stack frame 2:
///       parse arguments
///       push_args(drain from scratch buf)
///       scratch buf is now "reset" to where it was before we recursed
///     parse more arguments...
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ArgList {
    pub start: Index,
    pub len: Index,
}

impl ArgList {
    #[inline]
    pub fn len(&self) -> usize {
        self.len.into()
    }

    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.len.0 == 0
    }

    #[inline]
    fn as_range(&self) -> std::ops::Range<usize> {
        let start: usize = self.start.into();
        let end: usize = start + usize::from(self.len);
        std::ops::Range::<usize> { start, end }
    }
}

/// Instead of generating a tree of references or smart pointers (Rc), we'll
///  pack all expressions into a flat array and have them refer to each other by
///  an id (which is simply an index into that array).
///
/// Argument lists require a bit of special handling because we don't want
///  FunctionCall and ConcatOp to actually carry a Vec and own the subexpressions.
/// To handle these, all arguments get parsed as Expressions and stored in
///  [expressions](Self::expressions), then their ids are stored in
///  [arg_lists](Self::arg_lists).
/// A particular argument list is contiguous within [arg_lists](Self::arg_lists).
///  For example, when this expression is parsed:
///     fn_a(fn_b(1), 2)
///
/// This will get parsed into an expressions list:
///        id=0,                              id=1,     id=2,                              id=3
///  [Number(1), FunctionCall(fn_b, ArgList(0, 1)), Number(2), FunctionCall(fn_a, ArgList(1, 2))]
///
/// The two ArgLists reference spans of arg_lists:
///  [ExpressionId(0), ExpressionId(1), Expression(2)]
///
/// So fn_a's ArgList (1,2) is the span at arg_lists[1..1+2], that is, ExpressionIds 1 and 2.
/// These in turn map to FunctionCall(fn_b) and Number(2), which are indeed its two arguments.
#[derive(Clone, PartialEq)]
pub struct ExpressionTree<E> {
    /// All expressions are stored in a flat list. References are via ExpressionId.
    pub expressions: Vec<E>,
    /// All argument lists are stored packed contiguously
    pub arg_lists: Vec<ExpressionId>,
}

impl<E> ExpressionTree<E> {
    pub fn new() -> Self {
        Self {
            expressions: Vec::with_capacity(32),
            arg_lists: Vec::with_capacity(64),
        }
    }

    /// Create an ExpressionTree using previously allocated Vecs.
    pub fn new_from_vecs(expressions: Vec<E>, arg_lists: Vec<ExpressionId>) -> Self {
        Self {
            expressions,
            arg_lists,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.expressions.is_empty()
    }

    /// Clears the internal vectors, resetting this tree to a clean state. The
    ///  internal allocations and capacities are unaffected, making this an
    ///  efficient way to reuse the memory.
    pub fn clear(&mut self) {
        self.expressions.clear();
        self.arg_lists.clear();
    }

    /// Add `expr` to this tree, returning the ExpressionId
    pub fn push_expr(&mut self, expr: E) -> ExpressionId {
        let id = self.expressions.len().into();
        self.expressions.push(expr);
        id
    }

    /// Adds the `ids` to the internal argument list and returns an [ArgList]
    ///  that can be used with [get_args](Self::get_args) to later retrieve them.
    pub fn push_args(&mut self, ids: impl ExactSizeIterator<Item = ExpressionId>) -> ArgList {
        let start = self.arg_lists.len().into();
        let len = ids.len().into();
        self.arg_lists.extend(ids);
        ArgList { start, len }
    }

    /// Get the expression with `id`. This panics if `id` doesn't reference an
    ///  expression pushed to this tree.
    #[inline]
    pub fn get_expr_unchecked(&self, id: ExpressionId) -> &E {
        &self.expressions[usize::from(id)]
    }

    #[inline]
    pub fn get_expr(&self, id: ExpressionId) -> Option<&E> {
        self.expressions.get(usize::from(id))
    }

    /// Get the ExpressionIds representing a particular argument list
    #[inline]
    pub fn get_args(&self, list: &ArgList) -> &[ExpressionId] {
        &self.arg_lists[list.as_range()]
    }

    /// Get the root expression. This may be empty because the tree can be empty.
    #[inline]
    pub fn get_root(&self) -> Option<&E> {
        // Implementation note: the last expression is always the root. We know
        //  this to be true because any sub-expressions that it relies on must
        //  already have IDs.
        // More formally:
        //  - The only way for an expression to refer to another expression is
        //     by ID.
        //  - The only way for an expression to get an ID is to push it to the tree.
        //  - Therefore an expression cannot be pushed until all its
        //     sub-expressions have been pushed.
        //  - The root relies, directly or indirectly, on all other expressions.
        //  - Therefore the root is last.
        self.expressions.last()
    }

    #[inline]
    pub fn get_root_id(&self) -> Option<ExpressionId> {
        // The last expression is always the root (see [get_root] for reasoning)
        match self.expressions.len() {
            0 => None,
            n => Some(ExpressionId::from(n - 1)),
        }
    }
}

impl<E> Default for ExpressionTree<E> {
    fn default() -> Self {
        Self::new()
    }
}
