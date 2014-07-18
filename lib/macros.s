use prelude

`+` A B = [_mcall A '+' B]
`*` A B = [_mcall A '*' B]
`/` A B = [_mcall A '/' B]
`%` A B = [_mcall A '%' B]

export '+' '*' '/' '%'
