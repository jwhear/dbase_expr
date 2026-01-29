use std::str::FromStr;

#[derive(strum_macros::Display, Debug, Clone, PartialEq, Eq)]
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
    fn try_from(word: &[u8]) -> Result<Self, Self::Error> {
        match word.to_ascii_uppercase().as_slice() {
            b"ALLTRIM" => Ok(CodebaseFunction::ALLTRIM),
            b"CHR" => Ok(CodebaseFunction::CHR),
            b"CTOD" => Ok(CodebaseFunction::CTOD),
            b"DATE" => Ok(CodebaseFunction::DATE),
            b"DAY" => Ok(CodebaseFunction::DAY),
            b"DELETED" => Ok(CodebaseFunction::DELETED),
            b"DTOC" => Ok(CodebaseFunction::DTOC),
            b"DTOS" => Ok(CodebaseFunction::DTOS),
            b"EMPTY" => Ok(CodebaseFunction::EMPTY),
            b"IIF" => Ok(CodebaseFunction::IIF),
            b"LEFT" => Ok(CodebaseFunction::LEFT),
            b"LTRIM" => Ok(CodebaseFunction::LTRIM),
            b"MONTH" => Ok(CodebaseFunction::MONTH),
            b"PADL" => Ok(CodebaseFunction::PADL),
            b"RECNO" => Ok(CodebaseFunction::RECNO),
            b"RIGHT" => Ok(CodebaseFunction::RIGHT),
            b"RTRIM" => Ok(CodebaseFunction::RTRIM),
            b"STOD" => Ok(CodebaseFunction::STOD),
            b"STR" => Ok(CodebaseFunction::STR),
            b"SUBSTR" => Ok(CodebaseFunction::SUBSTR),
            b"TRIM" => Ok(CodebaseFunction::TRIM),
            b"UPPER" => Ok(CodebaseFunction::UPPER),
            b"VAL" => Ok(CodebaseFunction::VAL),
            b"YEAR" => Ok(CodebaseFunction::YEAR),
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
