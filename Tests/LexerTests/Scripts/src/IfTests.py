# Generates test files for the #if family of preprocessor directives.

def GenIfRecursion(n, file):
    # ABC(ABC(ABC(1))) == 4
    file.write("#define ABC(x) x + 1\n")
    file.write("#if ")
    
    for i in range(0, n):
        file.write("ABC(")
        
    file.write(str(1) + ")" * n)
    file.write(" == " + str(n + 1) + "\n")
    file.write("\tok\n")
    file.write("#else\n\tnot_ok\n")
    file.write("#endif");
    
    return

def GenIfRecursionExpect(n, file):
    file.write("ident # ok")
    return

def GetNestedIf(n, file):
    # Generates sequences of #ifdef-#else blocks of this form:
    # #define A1
    # #define A2
    #
    # #ifdef A1
    #    a1
    #    #ifdef A2
    #        a2
    #        #ifdef A3
    #            a3
    #            #ifdef A3
    #                a4
    #            #else
    #                b4
    #            #endif
    #        #else
    #            b3
    #        #endif
    #    #else
    #        b2
    #    #endif
    # #else
    #    b1
    # #endif
    # Tokens when lexing:
    # a1 a2 b3 (a1...an/2, bn/2+1)
    for i in range (0, n // 2):
        file.write("#define A" + str(i + 1) + "\n")
    
    for i in range (0, n):
        file.write(("\t" * i) + "#ifdef A" + str(i + 1) + "\n")
        file.write("\t" * (i + 1) + "a" + str(i + 1) + "\n")
    
    for i in range (0, n):
        file.write("\t" * (n-i-1) + "#else" + "\n")
        file.write("\t" * (n-i) + "b" + str(n - i) + "\n")
        file.write("\t" * (n-i-1) + "#endif" + "\n")
    
    return

def GetNestedIfExpect(n, file):
    for i in range(0, n // 2):
        file.write("ident # a" + str(i + 1) + "\n")
    
    file.write("ident # b" + str(n // 2 + 1) + "\n")        
    return

def Paste1(n, file):
    # Tests identifier pasting.
    # PASTE(a1, a2, ... 
    file.write("PASTE(")
    for i in range(0, n - 1):
        file.write("a" + str(i) + ",")
    file.write("a" + str(n - 1) + ")")
    
    return;

def Paste1Expect(n, file):
    file.write("ident # ")
    for i in range(0, n):
        file.write("a" + str(i))
    return

def Paste2(n, file):
    # Tests string pasting.
    file.write("PASTE(")
    for i in range(0, n - 1):
        file.write("\"a" + str(i) + "\",")
    file.write("\"a" + str(n - 1) + "\")")
    
    return;

def Paste2Expect(n, file):
    file.write("str # ")
    
    for i in range(0, n):
        file.write("a" + str(i))
    return

def Paste3(n, file):
    # Tests number pasting.
    file.write("PASTE(")
    for i in range(0, n - 1):
        file.write(str(i) + ",")
    file.write(str(n - 1) + ")")
    
    return;

def Paste3Expect(n, file):
    file.write("numb # ")
    
    for i in range(0, n):
        file.write(str(i))
    return

def Paste4(n, file):
    # Tests identifier and number pasting (should result identifiers).
    file.write("PASTE(")
    for i in range(0, n - 1):
        if i % 2:
            file.write(str(i) + ",")
        else:
            file.write("a" + str(i) + ",")
            
    file.write(str(n - 1) + ")")
    return;

def Paste4Expect(n, file):
    file.write("ident # ")
    
    for i in range(0, n - 1):
        if i % 2:
            file.write(str(i))
        else:
            file.write("a" + str(i))
            
    file.write(str(n - 1))
    return

def GenPaste(n, file):
    file.write("#define PASTE(")
    for i in range(0, n - 1):
        file.write("a" + str(i) + ",")
    file.write("a" + str(n - 1) + ")\\\n\t")
    
    for i in range(0, n-1):
        file.write("a" + str(i) + "##")
    file.write("a" + str(n - 1) + "\n")
    
    file.write("\n")
    tests = [Paste1, Paste2, Paste3, Paste4]
    for test in tests:
        test(n, file)
        file.write("\n")
        
    return

def GenPasteExpect(n, file):
    tests = [Paste1Expect, Paste2Expect, Paste3Expect, Paste4Expect]
    for test in tests:
        test(n, file)
        file.write("\n")
        
    return

# --------------------------------------------------
if __name__ == '__main__':
    tests = [(GenIfRecursion, GenIfRecursionExpect),
             (GetNestedIf, GetNestedIfExpect),
             (GenPaste, GenPasteExpect)]
    ct = 1;
    n = 100;
    
    for test, expect in tests:
        file = open("..//..//Data//test_auto" + str(ct) + ".txt", mode = "w")
        test(n, file)
        file.close()
        
        file = open("..//..//Data//expect_auto" + str(ct) + ".txt", mode = "w")
        expect(n, file)
        file.close()
        
        ct = ct + 1
    pass