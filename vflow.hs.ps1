cls
cd hs
ghc -o vflow Vflow
cd ..
echo ""
.\hs\vflow.exe bash $args[0]
echo ""
