cls
cd hs
ghc -o vflow Vflow
strip vflow.exe
cd ..
echo ""
.\hs\vflow.exe bash $args[0]
echo ""
