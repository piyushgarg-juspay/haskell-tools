ht-gateway +RTS -s -RTS print /home/dre/Desktop/piyush/euler-api-txns/euler-x/src-generated/Gateway/PayTMv2/ PayTMv2 -clear-package-db -package-db /nix/store/vqfqczb8nnj6xmm73w8idaxrpgya2d6p-ghc-8.8.4-with-packages/lib/ghc-8.8.4/package.conf.d -package-db /home/dre/Desktop/piyush/euler-api-txns/dist-newstyle/packagedb/ghc-8.8.4 -XRankNTypes -XPatternSynonyms -XExplicitNamespaces -XLambdaCase -XConstraintKinds -XFlexibleContexts -XDerivingStrategies -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses -XTypeFamilies -XUndecidableInstances -XDeriveGeneric -XPolyKinds -XTypeOperators -XExistentialQuantification -XTypeApplications -XMultiParamTypeClasses -XScopedTypeVariables -XFunctionalDependencies -XBlockArguments -XViewPatterns -XTemplateHaskell -XPackageImports -XNoImplicitPrelude -XDeriveFunctor -XAllowAmbiguousTypes -XOverloadedStrings -XPartialTypeSignatures -XOverloadedLabels -XDuplicateRecordFields -XNamedFieldPuns -XTupleSections -XUnicodeSyntax -XQuasiQuotes -XDeriveAnyClass -XEmptyCase -XStandaloneDeriving -freduction-depth=2000 -w
res=$(echo "$?")
if [[ $res == 0 ]]
then
echo "Completed"
else
echo "Loop break" 
./loop.sh
fi
