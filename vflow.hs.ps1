cls
cd hs
ghc -o vflow Vflow
$ghc_path = split-path (split-path (where.exe ghc))
& $ghc_path\mingw\bin\strip vflow.exe
cd ..
echo ""
.\hs\vflow.exe bash $args[0]
echo ""
