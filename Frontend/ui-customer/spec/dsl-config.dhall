let rootDir = env:GIT_ROOT_PATH

let rootPrefixes = [ rootDir ++ "/Frontend/ui-customer/src/" ]

let moduleMaps =
      [ { _1 = "BookAnyProps", _2 = "Components.ChooseYourRide.Controller" } ]

in  { _tdRootPaths = rootPrefixes, _defaultModuleMapper = moduleMaps }
