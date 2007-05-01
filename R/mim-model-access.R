DF.mim                <- function(x) x$modelInfo$DF
DF                    <- function(x) UseMethod("DF")
deviance.mim          <- function(object, ...) object$modelInfo$deviance
likelihood            <- function(x) x$modelInfo$likelihood

.cliques              <- function(x) x$modelInfo$Cliques ## letters
.is.fitted            <- function(x) ifelse (is.null(x$modelInfo), FALSE, x$modelInfo$Fitted)
.mimFormula           <- function(x) x$mimFormula
.mimFormula.letter    <- function(x) x$mimFormula.letter
.Formula.as.string    <- function(x) x$modelInfo$Formula.as.string
.DF                   <- function(x) x$modelInfo$DF
.likelihood           <- function(x) x$modelInfo$likelihood

.getgmData <- function(object) object$data

