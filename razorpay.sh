# ht-gateway +RTS -s -RTS print /Users/piyush.garg/ht-gateway-task/euler-api-txns/euler-x/src-generated/Gateway/Razorpay/ Razorpay -clear-package-db -package-db /nix/store/5gjrr6qww23j25wdyl1sm97s768myigf-ghc-8.8.4-with-packages/lib/ghc-8.8.4/package.conf.d -package-db /Users/piyush.garg/ht-gateway-task/euler-api-txns/dist-newstyle/packagedb/ghc-8.8.4 -XRankNTypes -XPatternSynonyms -XExplicitNamespaces -XLambdaCase -XConstraintKinds -XFlexibleContexts -XDerivingStrategies -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses -XTypeFamilies -XUndecidableInstances -XDeriveGeneric -XPolyKinds -XTypeOperators -XExistentialQuantification -XTypeApplications -XMultiParamTypeClasses -XScopedTypeVariables -XFunctionalDependencies -XBlockArguments -XViewPatterns -XTemplateHaskell -XPackageImports -XNoImplicitPrelude -XDeriveFunctor -XAllowAmbiguousTypes -XOverloadedStrings -XPartialTypeSignatures -XOverloadedLabels -XDuplicateRecordFields -XNamedFieldPuns -XTupleSections -XUnicodeSyntax -XQuasiQuotes -XDeriveAnyClass -XEmptyCase -XStandaloneDeriving -XRecordWildCards -freduction-depth=2000 -w
# ht-gateway +RTS -s -RTS print /Users/piyush.garg/ht-gateway-task/euler-api-txns/euler-x/src-generated/Gateway/Razorpay/ Razorpay -package-db /nix/store/5gjrr6qww23j25wdyl1sm97s768myigf-ghc-8.8.4-with-packages/lib/ghc-8.8.4/package.conf.d -package-db /Users/piyush.garg/ht-gateway-task/euler-api-txns/dist-newstyle/packagedb/ghc-8.8.4
ht-gateway +RTS -s -RTS print /Users/piyush.garg/ht-gateway-task/euler-api-txns/euler-x/src-generated/Gateway/Razorpay/ Razorpay -freduction-depth=2000 -package-db /nix/store/5gjrr6qww23j25wdyl1sm97s768myigf-ghc-8.8.4-with-packages/lib/ghc-8.8.4/package.conf.d
ht-gateway +RTS -s -RTS print /Users/piyush.garg/ht-gateway-task/euler-api-txns/euler-x/src-generated/Gateway/Paytm/ Paytm -freduction-depth=2000 -package-db /nix/store/5gjrr6qww23j25wdyl1sm97s768myigf-ghc-8.8.4-with-packages/lib/ghc-8.8.4/package.conf.d
res=$(echo "$?")
if [[ $res == 0 ]]
then
echo "Completed"
else
echo "Loop break" 
./razorpay.sh
fi