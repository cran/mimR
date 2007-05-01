
setOldClass("foo")

setMethod("edges", signature(object = "foo"),
          function(object, which) {
            print("edges - test (mimR)")
          })
