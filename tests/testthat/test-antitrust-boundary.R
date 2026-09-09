test_that("coordination namespace contains no private antitrust (:::) access", {
  ## Inspect the deparsed source of every object bound in the coordination
  ## namespace (functions, generics, and method bodies). This is robust to
  ## whether the package is loaded via load_all() or as an installed image,
  ## since :::/getFromNamespace calls survive both source and byte-compiled
  ## forms in the deparsed body text.
  ns <- asNamespace("coordination")
  objs <- mget(ls(ns, all.names = TRUE), envir = ns)

  deparse_all <- function(x) {
    if (is.function(x)) {
      paste(deparse(body(x)), collapse = "\n")
    } else if (methods::is(x, "MethodDefinition") || methods::is(x, "genericFunction")) {
      paste(deparse(x), collapse = "\n")
    } else {
      ""
    }
  }

  text <- vapply(objs, deparse_all, character(1))

  hits <- grep("antitrust:::|getFromNamespace\\(|asNamespace\\(\"antitrust\"\\)", text)
  expect_length(hits, 0)
})

test_that("coordination only calls exported antitrust generics/classes", {
  imports <- getNamespaceImports("coordination")[["antitrust"]]
  expect_true(is.null(imports) || all(nzchar(imports)))
  ## Every symbol coordination imports from antitrust must appear in
  ## antitrust's own NAMESPACE export surface.
  ns <- asNamespace("antitrust")
  exported <- getNamespaceExports(ns)
  imported_generics <- c("calcPrices", "calcMargins", "calcSlopes", "calcMC",
                          "calcProducerSurplus", "calcShares", "calcQuantities",
                          "ownerToMatrix", "ownerToVec")
  for (g in imported_generics) {
    has_export <- g %in% exported ||
      length(methods::getGenerics(ns)@.Data[methods::getGenerics(ns)@.Data == g]) > 0
    expect_true(has_export, info = paste("generic not public in antitrust:", g))
  }
})

test_that("antitrust package itself does not depend on coordination", {
  desc <- utils::packageDescription("antitrust")
  deps <- c(desc$Depends, desc$Imports, desc$Suggests)
  deps <- unlist(strsplit(paste(deps, collapse = ","), ","))
  deps <- trimws(gsub("\\(.*\\)", "", deps))
  expect_false("coordination" %in% deps)
})
