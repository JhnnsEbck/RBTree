# Compile the project
ghc -i./src -outputdir ./out -o ./out/Main ./src/Main.hs

# Run the program with the input file
./out/Main ./in/test.txt
