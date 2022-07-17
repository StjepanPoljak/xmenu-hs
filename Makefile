proj=xmenu

$(proj): Main.hs XMenuGlobal.hs XWindow.hs XRDB.hs XString.hs XLabel.hs XElementClass.hs XContext.hs XElement.hs XManagerClass.hs XList.hs XEvent.hs
	ghc -dynamic $^ -o $@

.phony=clean

clean:
	rm -rf *.o *.hi $(proj)
