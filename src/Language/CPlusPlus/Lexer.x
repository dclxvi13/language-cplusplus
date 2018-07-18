{
module Language.CPlusPlus.Lexer where
}

%wrapper "posn"

$cppWhite = [\ \t\f\v]

$hexDigit = [0-9a-fA-F]
$octDigit = [0-7]
@hexQuad = $hexDigit{4}

$nondigit = [a-zA-Z_]
$digit = [0-9]
$nonzero = [1-9]
$opPuncSymbol = [\~\!\@\#\$\%\^\&\*\(\)\[\]\{\}\-\=\+\\\|\;\'\:\"\,\.\/\<\>\?]

@sourceChar = $digit | $nondigit
@sourceCharSeq = (@sourceChar)+

@universalCharacterName = \\ [uU] @hexQuad
@lineterm = [\n\r] | \r\n

@lineComment = "//" .* @lineterm
@cstyleComment = "/*" .* "*/"
@comment = @lineComment | @cstyleComment

@identifier = $nondigit ($digit | $nondigit)*

$sign = [\+\-]
@digits = $digit+
@number = $digit* . $digit*
@expNumber = @number [eE] $sign @digits

@ppNumber = @expNumber | @number | @digits

@decimalLiteral = $nonzero $digit*
@octalLiteral = 0 $octDigit+
@hexadecimalLiteral = 0 [xX] $hexDigit+

$unsignedSuffix = [uU]
$longSuffix = [lL]
@longLongSuffix = ll | LL

@integerSuffix = ($unsignedSuffix @longLongSuffix?)
               | ($unsignedSuffix $longSuffix?)
               | (@longLongSuffix $unsignedSuffix?)
               | ($longSuffix $unsignedSuffix?)

@integerLiteral = (@decimalLiteral | @octalLiteral | @hexadecimalLiteral | 0) @integerSuffix?

@octEscape = [0123]? $octDigit{1, 2}
@hexEscape = \\x $hexDigit{1,2}
@escapeSequence = \\ (@octEscape | @hexEscape | [abfnrtv\'\"\?\\])

@cChar = @universalCharacterName | @escapeSequence | [^\'\\\n]

@charLiteral = [uUL]?\' @cChar \'

$floatingSuffix = [fFlL]

@exponentPart = [eE] $sign? @digits
@fractionalConstant = ($digit* "." @digits)|(@digits ".")

@floatingLiteral = (@digits @exponentPart $floatingSuffix?)
                 | (@fractionalConstant @exponentPart? $floatingSuffix?)

@encodingPrefix = [uUL]|u8

@sChar = @universalCharacterName | @escapeSequence | [^\"\\\n]

$dChar = [^\(\)\ \\\n\r\t\v\f]
$rChar = [^\)]

@rawString = \" $dChar* \( $rChar* \) $dChar* \"

@stringLiteral = (@encodingPrefix? \" @sChar* \")
               | (@encodingPrefix? "R" @rawString)


@userDefinedIntegerLiteral = (@decimalLiteral @identifier)
                           | (@octalLiteral @identifier)
                           | (@hexadecimalLiteral @identifier)

@userDefinedFloatingLiteral = (@fractionalConstant @exponentPart? @identifier)
                            | (@digits @exponentPart @identifier)

@userDefinedCharLiteral = @charLiteral @identifier

@userDefinedStringLiteral = @stringLiteral @identifier

@userDefinedLiteral = @userDefinedIntegerLiteral
                    | @userDefinedFloatingLiteral
                    | @userDefinedCharLiteral
                    | @userDefinedStringLiteral

tokens :-
    $cppWhite+          ;
    \\@lineterm         ;
    @lineComment        { \p s -> L (pos p) $ TLineComment s            }
    @cstyleComment      { \p s -> L (pos p) $ TComment s        }
    @lineterm           { \p s -> L (pos p) $ EOL                   }

    alignas             { \p _ -> L (pos p) $ KW_Alignas            }
    alignof             { \p _ -> L (pos p) $ KW_Alignof            }
    asm                 { \p _ -> L (pos p) $ KW_Asm                }
    auto                { \p _ -> L (pos p) $ KW_Auto               }
    bool                { \p _ -> L (pos p) $ KW_Bool               }
    break               { \p _ -> L (pos p) $ KW_Break              }
    case                { \p _ -> L (pos p) $ KW_Case               }
    catch               { \p _ -> L (pos p) $ KW_Catch              }
    char                { \p _ -> L (pos p) $ KW_Char               }
    char16_t            { \p _ -> L (pos p) $ KW_Char16T            }
    char32_t            { \p _ -> L (pos p) $ KW_Char32T            }
    class               { \p _ -> L (pos p) $ KW_Class              }
    const               { \p _ -> L (pos p) $ KW_Const              }
    constexpr           { \p _ -> L (pos p) $ KW_Constexpr          }
    const_cast          { \p _ -> L (pos p) $ KW_ConstCast          }
    continue            { \p _ -> L (pos p) $ KW_Continue           }
    decltype            { \p _ -> L (pos p) $ KW_Decltype           }
    default             { \p _ -> L (pos p) $ KW_Default            }
    delete              { \p _ -> L (pos p) $ KW_Delete             }
    do                  { \p _ -> L (pos p) $ KW_Do                 }
    double              { \p _ -> L (pos p) $ KW_Double             }
    dynamic_cast        { \p _ -> L (pos p) $ KW_DynamicCast        }
    else                { \p _ -> L (pos p) $ KW_Else               }
    enum                { \p _ -> L (pos p) $ KW_Enum               }
    explicit            { \p _ -> L (pos p) $ KW_Explicit           }
    export              { \p _ -> L (pos p) $ KW_Export             }
    extern              { \p _ -> L (pos p) $ KW_Extern             }
    float               { \p _ -> L (pos p) $ KW_Float              }
    for                 { \p _ -> L (pos p) $ KW_For                }
    friend              { \p _ -> L (pos p) $ KW_Friend             }
    goto                { \p _ -> L (pos p) $ KW_Goto               }
    if                  { \p _ -> L (pos p) $ KW_If                 }
    inline              { \p _ -> L (pos p) $ KW_Inline             }
    int                 { \p _ -> L (pos p) $ KW_Int                }
    long                { \p _ -> L (pos p) $ KW_Long               }
    mutable             { \p _ -> L (pos p) $ KW_Mutable            }
    namespace           { \p _ -> L (pos p) $ KW_Namespace          }
    new                 { \p _ -> L (pos p) $ KW_New                }
    noexcept            { \p _ -> L (pos p) $ KW_Noexcept           }
    operator            { \p _ -> L (pos p) $ KW_Operator           }
    private             { \p _ -> L (pos p) $ KW_Private            }
    protected           { \p _ -> L (pos p) $ KW_Protected          }
    public              { \p _ -> L (pos p) $ KW_Public             }
    register            { \p _ -> L (pos p) $ KW_Register           }
    reinterpret_cast    { \p _ -> L (pos p) $ KW_ReinterpretCast    }
    return              { \p _ -> L (pos p) $ KW_Return             }
    short               { \p _ -> L (pos p) $ KW_Short              }
    signed              { \p _ -> L (pos p) $ KW_Signed             }
    sizeof              { \p _ -> L (pos p) $ KW_Sizeof             }
    static              { \p _ -> L (pos p) $ KW_Static             }
    static_assert       { \p _ -> L (pos p) $ KW_StaticAssert       }
    static_cast         { \p _ -> L (pos p) $ KW_StaticCast         }
    struct              { \p _ -> L (pos p) $ KW_Struct             }
    switch              { \p _ -> L (pos p) $ KW_Switch             }
    template            { \p _ -> L (pos p) $ KW_Template           }
    this                { \p _ -> L (pos p) $ KW_This               }
    thread_local        { \p _ -> L (pos p) $ KW_ThreadLocal        }
    throw               { \p _ -> L (pos p) $ KW_Throw              }
    try                 { \p _ -> L (pos p) $ KW_Try                }
    typedef             { \p _ -> L (pos p) $ KW_Typedef            }
    typeid              { \p _ -> L (pos p) $ KW_Typeid             }
    typename            { \p _ -> L (pos p) $ KW_Typename           }
    union               { \p _ -> L (pos p) $ KW_Union              }
    unsigned            { \p _ -> L (pos p) $ KW_Unsigned           }
    using               { \p _ -> L (pos p) $ KW_Using              }
    virtual             { \p _ -> L (pos p) $ KW_Virtual            }
    void                { \p _ -> L (pos p) $ KW_Void               }
    volatile            { \p _ -> L (pos p) $ KW_Volatile           }
    wchar_t             { \p _ -> L (pos p) $ KW_WCharT             }
    while               { \p _ -> L (pos p) $ KW_While              }

    true                { \p s -> L (pos p) $ Literal_Boolean s     }
    false               { \p s -> L (pos p) $ Literal_Boolean s     }

    nullptr             { \p s -> L (pos p) $ Literal_NullPtr s     }

    "{"                 { \p _ -> L (pos p) $ Punc_LeftBrace        }
    "}"                 { \p _ -> L (pos p) $ Punc_RightBrace       }
    "["                 { \p _ -> L (pos p) $ Punc_LeftBracket      }
    "]"                 { \p _ -> L (pos p) $ Punc_RightBracket     }
    "#"                 { \p _ -> L (pos p) $ Punc_Hash             }
    "##"                { \p _ -> L (pos p) $ Punc_DoubleHash       }
    "("                 { \p _ -> L (pos p) $ Punc_LeftParen        }
    ")"                 { \p _ -> L (pos p) $ Punc_RightParen       }
    "<:"                { \p _ -> L (pos p) $ Punc_LeftBracket      }
    ":>"                { \p _ -> L (pos p) $ Punc_RightBracket     }
    "<%"                { \p _ -> L (pos p) $ Punc_LeftBrace        }
    "%>"                { \p _ -> L (pos p) $ Punc_RightBrace       }
    "%:"                { \p _ -> L (pos p) $ Punc_Hash             }
    "%:%:"              { \p _ -> L (pos p) $ Punc_DoubleHash       }
    ";"                 { \p _ -> L (pos p) $ Punc_Semi             }
    ":"                 { \p _ -> L (pos p) $ Punc_Colon            }
    "..."               { \p _ -> L (pos p) $ Punc_ThreeDot         }
    "?"                 { \p _ -> L (pos p) $ Punc_QuestionMark     }
    "::"                { \p _ -> L (pos p) $ Punc_DoubleColon      }
    "."                 { \p _ -> L (pos p) $ Punc_Dot              }
    ".*"                { \p _ -> L (pos p) $ Op_DotPtr             }
    "+"                 { \p _ -> L (pos p) $ Op_Plus               }
    "-"                 { \p _ -> L (pos p) $ Op_Minus              }
    "*"                 { \p _ -> L (pos p) $ Op_Mul                }
    "/"                 { \p _ -> L (pos p) $ Op_Div                }
    "%"                 { \p _ -> L (pos p) $ Op_Rem                }
    "^"                 { \p _ -> L (pos p) $ Op_Xor                }
    "&"                 { \p _ -> L (pos p) $ Op_And                }
    "|"                 { \p _ -> L (pos p) $ Op_Or                 }
    "~"                 { \p _ -> L (pos p) $ Op_Tilda              }
    "!"                 { \p _ -> L (pos p) $ Op_Not                }
    "="                 { \p _ -> L (pos p) $ Op_Assign             }
    "<"                 { \p _ -> L (pos p) $ Op_Less               }
    ">"                 { \p _ -> L (pos p) $ Op_Greater            }
    "+="                { \p _ -> L (pos p) $ Op_AssignPlus         }
    "-="                { \p _ -> L (pos p) $ Op_AssignMinus        }
    "*="                { \p _ -> L (pos p) $ Op_AssignMul          }
    "/="                { \p _ -> L (pos p) $ Op_AssignDiv          }
    "%="                { \p _ -> L (pos p) $ Op_AssignRem          }
    "^="                { \p _ -> L (pos p) $ Op_AssignXor          }
    "&="                { \p _ -> L (pos p) $ Op_AssignAnd          }
    "|="                { \p _ -> L (pos p) $ Op_AssignOr           }
    "<<"                { \p _ -> L (pos p) $ Op_LeftShift          }
    ">>"                { \p _ -> L (pos p) $ Op_RightShift         }
    "<<="               { \p _ -> L (pos p) $ Op_AssignLeftShift    }
    ">>="               { \p _ -> L (pos p) $ Op_AssignRightShift   }
    "=="                { \p _ -> L (pos p) $ Op_Eq                 }
    "!="                { \p _ -> L (pos p) $ Op_NotEq              }
    "<="                { \p _ -> L (pos p) $ Op_LessEq             }
    ">="                { \p _ -> L (pos p) $ Op_GreaterEq          }
    "&&"                { \p _ -> L (pos p) $ Op_LogicalAnd         }
    "||"                { \p _ -> L (pos p) $ Op_LogicalOr          }
    "++"                { \p _ -> L (pos p) $ Op_Increment          }
    "--"                { \p _ -> L (pos p) $ Op_Decrement          }
    ","                 { \p _ -> L (pos p) $ Punc_Comma            }
    "->*"               { \p _ -> L (pos p) $ Op_ArrowPtr           }
    "->"                { \p _ -> L (pos p) $ Op_Arrow              }
    and                 { \p _ -> L (pos p) $ Op_LogicalAnd         }
    and_eq              { \p _ -> L (pos p) $ Op_AssignAnd          }
    bitand              { \p _ -> L (pos p) $ Op_And                }
    bitor               { \p _ -> L (pos p) $ Op_Or                 }
    compl               { \p _ -> L (pos p) $ Op_Tilda              }
    not                 { \p _ -> L (pos p) $ Op_Not                }
    not_eq              { \p _ -> L (pos p) $ Op_NotEq              }
    or                  { \p _ -> L (pos p) $ Op_LogicalOr          }
    or_eq               { \p _ -> L (pos p) $ Op_AssignOr           }
    xor                 { \p _ -> L (pos p) $ Op_Xor                }
    xor_eq              { \p _ -> L (pos p) $ Op_AssignXor          }

    "#if"               { \p _ -> L (pos p) $ PP_If                 }
    "#ifdef"            { \p _ -> L (pos p) $ PP_Ifdef              }
    "#ifndef"           { \p _ -> L (pos p) $ PP_Ifndef             }
    "#elif"             { \p _ -> L (pos p) $ PP_Elif               }
    "#else"             { \p _ -> L (pos p) $ PP_Else               }
    "#endif"            { \p _ -> L (pos p) $ PP_Endif              }
    "#include"          { \p _ -> L (pos p) $ PP_Include            }
    "#define"           { \p _ -> L (pos p) $ PP_Define             }
    "#undef"            { \p _ -> L (pos p) $ PP_Undef              }
    "#line"             { \p _ -> L (pos p) $ PP_Line               }
    "#error"            { \p _ -> L (pos p) $ PP_Error              }
    "#pragma"           { \p _ -> L (pos p) $ PP_Pragma             }


    @integerLiteral     { \p s -> L (pos p) $ Literal_Integer s     }
    @charLiteral        { \p s -> L (pos p) $ Literal_Char s        }
    @floatingLiteral    { \p s -> L (pos p) $ Literal_Float s       }
    @stringLiteral      { \p s -> L (pos p) $ Literal_String s      }
    @userDefinedLiteral { \p s -> L (pos p) $ Literal_UserDefined s }

    @identifier         { \p s -> L (pos p) $ Id s                  }

{
data L a = L Pos a
  deriving (Show, Eq)

-- (line, column)
type Pos = (Int, Int)

pos :: AlexPosn -> Pos
pos (AlexPn _ l c) = (l,c)

data Token
    = KW_Alignas
    | KW_Alignof
    | KW_Asm
    | KW_Auto
    | KW_Bool
    | KW_Break
    | KW_Case
    | KW_Catch
    | KW_Char
    | KW_Char16T
    | KW_Char32T
    | KW_Class
    | KW_Const
    | KW_Constexpr
    | KW_ConstCast
    | KW_Continue
    | KW_Decltype
    | KW_Default
    | KW_Delete
    | KW_Do
    | KW_Double
    | KW_DynamicCast
    | KW_Else
    | KW_Enum
    | KW_Explicit
    | KW_Export
    | KW_Extern
    | KW_False
    | KW_Float
    | KW_For
    | KW_Friend
    | KW_Goto
    | KW_If
    | KW_Inline
    | KW_Int
    | KW_Long
    | KW_Mutable
    | KW_Namespace
    | KW_New
    | KW_Noexcept
    | KW_Nullptr
    | KW_Operator
    | KW_Private
    | KW_Protected
    | KW_Public
    | KW_Register
    | KW_ReinterpretCast
    | KW_Return
    | KW_Short
    | KW_Signed
    | KW_Sizeof
    | KW_Static
    | KW_StaticAssert
    | KW_StaticCast
    | KW_Struct
    | KW_Switch
    | KW_Template
    | KW_This
    | KW_ThreadLocal
    | KW_Throw
    | KW_True
    | KW_Try
    | KW_Typedef
    | KW_Typeid
    | KW_Typename
    | KW_Union
    | KW_Unsigned
    | KW_Using
    | KW_Virtual
    | KW_Void
    | KW_Volatile
    | KW_WCharT
    | KW_While

    | Punc_LeftBrace
    | Punc_RightBrace
    | Punc_LeftBracket
    | Punc_RightBracket
    | Punc_Hash
    | Punc_DoubleHash
    | Punc_LeftParen
    | Punc_RightParen
    | Punc_Semi
    | Punc_Colon
    | Punc_ThreeDot
    | Punc_QuestionMark
    | Punc_DoubleColon
    | Punc_Dot
    | Punc_Comma

    | Op_DotPtr
    | Op_Plus
    | Op_Minus
    | Op_Mul
    | Op_Div
    | Op_Rem
    | Op_Xor
    | Op_And
    | Op_Or
    | Op_Tilda
    | Op_Not
    | Op_Assign
    | Op_Less
    | Op_Greater
    | Op_AssignPlus
    | Op_AssignMinus
    | Op_AssignMul
    | Op_AssignDiv
    | Op_AssignRem
    | Op_AssignXor
    | Op_AssignAnd
    | Op_AssignOr
    | Op_LeftShift
    | Op_RightShift
    | Op_AssignLeftShift
    | Op_AssignRightShift
    | Op_Eq
    | Op_NotEq
    | Op_LessEq
    | Op_GreaterEq
    | Op_LogicalAnd
    | Op_LogicalOr
    | Op_Increment
    | Op_Decrement
    | Op_ArrowPtr
    | Op_Arrow
{-    | Op_LiterAnd
    | Op_LiterAndEq
    | Op_LiterBitAnd
    | Op_LiterBitOr
    | Op_LiterCompl
    | Op_LiterNot
    | Op_LiterNotEq
    | Op_LiterOr
    | Op_LiterOrEq
    | Op_LiterXor
    | Op_LiterXorEq -}

    | Literal_Integer String
    | Literal_Char String
    | Literal_String String
    | Literal_Float String
    | Literal_Boolean String
    | Literal_NullPtr String
    | Literal_UserDefined String

    | Id String

    | PP_If
    | PP_Ifdef
    | PP_Ifndef
    | PP_Elif
    | PP_Else
    | PP_Endif
    | PP_Include
    | PP_Define
    | PP_Undef
    | PP_Line
    | PP_Error
    | PP_Pragma

    | TComment String
    | TLineComment String
    | EOL
    deriving (Show, Eq)

lexer = alexScanTokens
}