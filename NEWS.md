### NEWS

#### V0.1.1

- `internals` now implemented in C++ _via_ `Rcpp` thanks to Dr Jasen Finch (@jasenfinch)

#### v0.1.0

- Implemented Strategy-1 from _Konukoglu,E. and Ganz,M.,2014_.  __Approximate false positive rate control in selection frequency for random forest__

- Support for `randomForest` and `ranger` forest objects

- Calculate selection frequency threshold for a given false positive rate (alpha)

- False positive rate feature selection

- Wrapper for selection frequencies extract from objects
