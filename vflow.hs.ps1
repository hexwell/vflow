cls
cd hs
ghc -o vflow Vflow
$ghc_path = Split-Path -parent (Split-Path -parent (where.exe ghc))
& $ghc_path\mingw\bin\strip vflow.exe
cd ..
echo ""
.\hs\vflow.exe bash $args[0]
echo ""
