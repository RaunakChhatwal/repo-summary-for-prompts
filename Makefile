a.out: main.hs
	ghc -o a.out -no-keep-hi-files -no-keep-o-files main.hs