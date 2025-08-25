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
