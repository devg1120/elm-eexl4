module Test.Test05 exposing (script)

script = """

use strjoin;
use debug;

let result  = strjoin("abcd", "XYZ");

let result2 = debug("abcd", "XYZ");

// test1
let x1 = 1 + 3 + (7 // 2);
let x2 = 1 + 3 
               + (7 // 2);

let x3 = -1 + 5 ;
let x4 = -1 * 5 ;

let z1 = True && False ;
let z2 = True || True ;
  
"""


