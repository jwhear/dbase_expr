use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CodebaseFunction {
    ALLTRIM,
    CHR,
    CTOD,
    DATE,
    DAY,
    DELETED,
    DTOC,
    DTOS,
    EMPTY,
    IIF,
    LEFT,
    LTRIM,
    MONTH,
    PADL,
    RECNO,
    RIGHT,
    RTRIM,
    STOD,
    STR,
    SUBSTR,
    TRIM,
    UPPER,
    VAL,
    YEAR,
    Unknown(String),
}

impl From<String> for CodebaseFunction {
    fn from(s: String) -> Self {
        s.parse().unwrap_or(CodebaseFunction::Unknown(s))
    }
}

impl TryFrom<&[u8]> for CodebaseFunction {
    type Error = ();
    //TODO need to benchmark this; the fancy stuff hurts maintainability
    // and might not be worth it once the optimizer gets at the simpler version
    fn try_from(word: &[u8]) -> Result<Self, Self::Error> {
        // For an explanation of this dark magic, read lex::consume_literal
        let value = unsafe { std::ptr::read_unaligned(word.as_ptr() as *const u64) };
        let value = crate::lex::lowercase_u64(value);
        let mask = if word.len() == 8 {
            u64::MAX
        } else {
            (1u64 << (word.len() * 8)) - 1
        };
        match value & mask {
            0x6d6972746c6c61 => Ok(CodebaseFunction::ALLTRIM),
            0x726863 => Ok(CodebaseFunction::CHR),
            0x646f7463 => Ok(CodebaseFunction::CTOD),
            0x65746164 => Ok(CodebaseFunction::DATE),
            0x796164 => Ok(CodebaseFunction::DAY),
            0x646574656c6564 => Ok(CodebaseFunction::DELETED),
            0x636f7464 => Ok(CodebaseFunction::DTOC),
            0x736f7464 => Ok(CodebaseFunction::DTOS),
            0x7974706d65 => Ok(CodebaseFunction::EMPTY),
            0x666969 => Ok(CodebaseFunction::IIF),
            0x7466656c => Ok(CodebaseFunction::LEFT),
            0x6d6972746c => Ok(CodebaseFunction::LTRIM),
            0x68746e6f6d => Ok(CodebaseFunction::MONTH),
            0x6c646170 => Ok(CodebaseFunction::PADL),
            0x6f6e636572 => Ok(CodebaseFunction::RECNO),
            0x7468676972 => Ok(CodebaseFunction::RIGHT),
            0x6d69727472 => Ok(CodebaseFunction::RTRIM),
            0x646f7473 => Ok(CodebaseFunction::STOD),
            0x727473 => Ok(CodebaseFunction::STR),
            0x727473627573 => Ok(CodebaseFunction::SUBSTR),
            0x6d697274 => Ok(CodebaseFunction::TRIM),
            0x7265707075 => Ok(CodebaseFunction::UPPER),
            0x6c6176 => Ok(CodebaseFunction::VAL),
            0x72616579 => Ok(CodebaseFunction::YEAR),
            _ => Err(()),
        }
    }
}

impl FromStr for CodebaseFunction {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_uppercase().as_str() {
            "ALLTRIM" => Ok(CodebaseFunction::ALLTRIM),
            "CHR" => Ok(CodebaseFunction::CHR),
            "CTOD" => Ok(CodebaseFunction::CTOD),
            "DATE" => Ok(CodebaseFunction::DATE),
            "DAY" => Ok(CodebaseFunction::DAY),
            "DELETED" => Ok(CodebaseFunction::DELETED),
            "DTOC" => Ok(CodebaseFunction::DTOC),
            "DTOS" => Ok(CodebaseFunction::DTOS),
            "EMPTY" => Ok(CodebaseFunction::EMPTY),
            "IIF" => Ok(CodebaseFunction::IIF),
            "LEFT" => Ok(CodebaseFunction::LEFT),
            "LTRIM" => Ok(CodebaseFunction::LTRIM),
            "MONTH" => Ok(CodebaseFunction::MONTH),
            "PADL" => Ok(CodebaseFunction::PADL),
            "RECNO" => Ok(CodebaseFunction::RECNO),
            "RIGHT" => Ok(CodebaseFunction::RIGHT),
            "RTRIM" => Ok(CodebaseFunction::RTRIM),
            "STOD" => Ok(CodebaseFunction::STOD),
            "STR" => Ok(CodebaseFunction::STR),
            "SUBSTR" => Ok(CodebaseFunction::SUBSTR),
            "TRIM" => Ok(CodebaseFunction::TRIM),
            "UPPER" => Ok(CodebaseFunction::UPPER),
            "VAL" => Ok(CodebaseFunction::VAL),
            "YEAR" => Ok(CodebaseFunction::YEAR),
            t => Err(format!("Unknown function: {}", t)),
        }
    }
}
