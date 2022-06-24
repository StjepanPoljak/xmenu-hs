proj=xmenu

$(proj): Main.hs XMenuGlobal.hs XWindow.hs XRDB.hs XString.hs XLabel.hs XElement.hs XContext.hs XManager.hs
	ghc -dynamic $^ -o $@

.phony=clean

clean:
	rm -rf *.o *.hi $(proj)
