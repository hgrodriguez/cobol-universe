DYNAMIC_LIBS=cards.dylib foundation.dylib stock.dylib tableau.dylib
TEST_LIBS=testcards.dylib testfoundation.dylib teststock.dylib testtableau.dylib
TEST_DRIVERS=testcards.exe testfoundation.exe teststock.exe testtableau.exe
TEST_ALL=testall.exe

all: $(DYNAMIC_LIBS) $(TEST_LIBS) $(TEST_DRIVERS) $(TEST_ALL)

clean:
	rm -f *.dylib *.exe

# all modules
cards.dylib: cards.cob
	cobc -m $<

foundation.dylib: foundation.cob
	cobc -m $<

stock.dylib: stock.cob
	cobc -m $<

tableau.dylib: tableau.cob
	cobc -m $<

# all single drivers as executables
testcards.exe: testcards.cob
	cobc -x -o testcards.exe $<

testfoundation.exe: testfoundation.cob foundation.dylib cards.dylib
	cobc -x -o testfoundation.exe $<

teststock.exe: teststock.cob stock.dylib cards.dylib
	cobc -x -o teststock.exe $<

testtableau.exe: testtableau.cob tableau.dylib stock.dylib cards.dylib
	cobc -x -o testtableau.exe $<

# all single drivers as dylibs
testcards.dylib: testcards.cob
	cobc -m $<

testfoundation.dylib: testfoundation.cob
	cobc -m $<

teststock.dylib: teststock.cob
	cobc -m $<

testtableau.dylib: testtableau.cob
	cobc -m $<

# all test driver
testall.exe: testall.cob
	cobc -x -o testall.exe testall.cob

