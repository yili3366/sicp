The expression

(car ''abracadabra)

is read by the interpreter as

(car (quote (quote abracadabra)))

or, more clearly illustrated using the quotation mark in place of the first quote special form, as

(car '(quote abracadabra))

The argument to car is the list (quote abracadabra) and its first element is the symbol quote, so that's the value that car returns.
