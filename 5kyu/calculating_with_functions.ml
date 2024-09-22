let zero = function f -> f 0
let one = function f -> f 1
let two = function f -> f 2
let three = function f -> f 3
let four = function f -> f 4
let five = function f -> f 5
let six = function f -> f 6
let seven = function f -> f 7
let eight = function f -> f 8
let nine = function f -> f 9


let plus n = function m -> m + (n Fun.id)

let minus n = function m -> m - (n Fun.id)

let times n = function m -> m * (n Fun.id)

let divided_by n = function m -> m / (n Fun.id)
