"use strict";

// module Main

exports.scrollBottomImpl = function(a) {
    // console.log("did run" + a);

       var elem = document.getElementById("packets");
       if (elem) {
           elem.scrollTo(0, elem.scrollHeight)
       }
 
    return a;
};
