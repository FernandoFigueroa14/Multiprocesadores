#Tarea 1 - Simple CPU Asembler
#Multiprocesadores (Grupo 1)

#Instruction Set

# Name	Opcode	Instruction	Description
# Load	0000	MOV Ra, d	Load data from memory location d into register a.
# Store	0001	MOV d, Ra	Store data from register a into memory location d.
# Add	0010	ADD Ra, Rb, Rc	Add register b and register c and store the result in register a.
# Load Constant	0011	MOV Ra, #c	Load the constant c into register a.
# Subtract	0100	SUB Ra, Rb, Rc	Subtract register c from register b (RF[b] - R[c]) and store the result in register a.
# Jump if zero	0101	JMPZ Ra, offset	Set the program counter (PC) to PC + offset if register a is 0.
# Assembly code notes: Each instruction should end with ";". Any instruction may begin with a label-- "label:".
# Constants must be decimal numbers.
#
# Your compiler should be able to support comments in the form of    //

# Creadores de código:
#Edgar Alexis González Amador A01746540
#Carlos Pano Hernández A01066264
# Luis Fernando Figueroa Rendón A01746139
#--------------------------------------------------------
#Programa para compilar un código en lenguaje ensamblador
#y devolver su equivalente en código máquina

import ply.lex as lex
import ply.yacc as yacc



#lista con las palabras reservadas y sus tokens
palabras_reservadas = {
                    'MOV' : 'MOV',
                    'ADD' : 'ADD',
                    'SUB' : 'SUB',
                    'JMPZ' : 'JMPZ',
}

#tokens del lenguaje ensamblador
tokens = [
    'ENTERO',
    'COMENTARIOS',
    'ESPACIOS_BLANCO',
    'REGISTRO',
    'PUNTOYCOMA',
    'COMA',
    'HASHTAG',
    'DOSPUNTOS',
    'LABEL'
] + list(palabras_reservadas.values())

# Especificacion de tokens
t_PUNTOYCOMA = r'\;'
t_COMA = r'\,'
t_HASHTAG = r'\#'
t_DOSPUNTOS = r'\:'

#funciones para definir los tokens mas complejos
def t_ENTERO(t):
    r'\d+'
    t.value = int(t.value)
    return t

#Se identifican los registros
def t_REGISTRO(t):
    r'[R][0-9]+'
    t.value = (t.value)
    return t

#se identifican las palabras reservadas
def t_ID(t):
    r'MOV|ADD|SUB|JMPZ'
    t.type = palabras_reservadas.get(t.value, 'ID')
    return t

#se identifican las etiquetas de tipo label
def t_LABEL(t):
    r'\w+'
    t.value = (t.value)
    return t

#se identifican todos los tipos de espacios y caracteres en blanco y se ignoran
def t_ESPACIOS_BLANCO(t):
    r' \s|\t|\n|\f|\r|\v'
    t.value = (t.value)

#se identifican los comentarios y se ignoran
def t_COMENTARIOS(t):
    r'[/]{2}[^\n]*\n*'
    t.value = (t.value)
    #return t

t=0; #se genera etiqueta

#token para ignorar los espacios y saltos de linea
t_ignore = ' \t\n'

#Función para detectar los errores
def t_error(t):
    print("Caracter ilegal '%s' " % t.value[0])
    t.lexer.skip(1)

#se inicia el analizador lexico
lexer=lex.lex()

#se pide un programa de entrada escrito en Ensamblador
entradag=input('Dame un programa en ensamblador (sin extensión):')
entrada = open(entradag + '.txt','r')

e=entrada.read()
lexer.input(e)
#lexer.input(entrada)

#se genera todo el analisis lexico
while True:
    tok = lexer.token()
    if not tok:
        break
    #print(tok)

#definicion de todas las derivaciones posibles del lenguaje Ensamblador
tabla={}
ins = 0
instrucción='Instruccion ' + str(ins)
var=0
numvar=0

#instrucciones
def p_instrucciones(p):
    '''instrucciones : lista_instrucciones instrucciones
                    | lista_instrucciones'''

    if(len(p)==1):
        p[0]=('instrucciones',p[1],p[2]) #AST

    else:
        p[0]=('instruccion',p[1]) #AST

#lista instrucciones
def p_lista_instrucciones(p):
    '''lista_instrucciones :  mov
                            | add
                            | substract
                            | jumpifzero
                            | label'''
    global ins, instrucción
    p[0] = p[1]
    tabla[instrucción]=p[1]
    ins = ins + 1
    instrucción = 'Instruccion ' + str(ins)

#Load:: MOV Ra,d | #Load constant:: MOV Ra,#c | #Store:: MOV d,Ra
def p_mov(p):
    '''mov : MOV REGISTRO COMA ENTERO PUNTOYCOMA
            | MOV REGISTRO COMA HASHTAG ENTERO PUNTOYCOMA
            | MOV ENTERO COMA REGISTRO PUNTOYCOMA'''

    global ins, instrucción

    if(type(p[4]) is int):
        #print('load from memory location ',p[4],' in register ',p[2])#AST
        p[0] = ('load', p[2], p[4])
    elif(type(p[2]) is int):
        #print('store in memory location ', p[2], ' in register ', p[4])  # AST
        p[0] = ('store', p[4], p[2])
    else:
        #print ('load_constant ', p[5], ' in register ', p[2])  # AST
        p[0] = ('load_constant', p[2], p[5])

#Add:: ADD Ra,Rb, Rc
def p_add(p):
    '''add : ADD REGISTRO COMA REGISTRO COMA REGISTRO PUNTOYCOMA'''
    #print ('Add register ', p[4],' and register ', p[6],' and store the result in register ', p[2])
    p[0] = ('add', p[2], p[4], p[6])

#Substract:: SUB Ra,Rb, Rc
def p_substract(p):
    '''substract : SUB REGISTRO COMA REGISTRO COMA REGISTRO PUNTOYCOMA'''
    #print('Substract register ', p[6],' from register ', p[4],' and store the result in register ', p[2])
    p[0] = ('sub', p[2], p[4], p[6])

#Jump if Zero:: JMPZ Ra,Offset
def p_jumpifzero(p):
    '''jumpifzero : JMPZ REGISTRO COMA LABEL PUNTOYCOMA'''
    #print('Jump to ', p[4], ' if register ', p[2], ' is zero')
    #p[0] = ('jmpz', p[2], p[4])
    p[0] = ('jmpz', p[2])

#Label:: LABEL: instruccion
def p_label(p):
    '''label : LABEL DOSPUNTOS mov
                    | LABEL DOSPUNTOS add
                    | LABEL DOSPUNTOS substract'''
    #print('Label ', p[1])
    p[0] = p[3]


#produccion vacia
def p_empty(p):
    'empty : '
    pass

#produccion que muestra el error sintactico
def p_error(p):
    print('Syntax error in Input!')
    print ('EL error sintáctico está en: ', p)

parser = yacc.yacc() #creacion del analizador sintáctico

AST =parser.parse(e)
#print (tabla)

salida=open(entradag+'.mc','w') #.mc = machine code


# Diccionario de operaciones con su contraparte en bytecode
dicopcode={ 'load':'0000', 'store':'0001', 'add':'0010', 'load_constant':'0011', 'sub':'0100', 'jmpz':'0101'}

#Diccionario con registros
dicreg={'R0':'0000', 'R1':'0001', 'R2':'0010', 'R3':'0011', 'R4':'0100', 'R5':'0101', 'R6':'0110', 'R7':'0111', 'R8':'1000', 'R9':'1001', 'R10':'1010', 'R11':'1011', 'R12':'1100', 'R13':'1101', 'R14':'1110', 'R15':'1111'}


#Tabla de instrucciones con machine code
print('{0}{1:15s}{0} {0}{2:13s}{0} {0}{3:6s}{0} {0}{4:9s}{0} {0}{5:8s}{0} {0}{6:9s}{0} {0}{7:8s}{0} {0}{8:9s}{0} {0}{9:8s}{0}'.format('|', 'Instrucción', 'TIPO', 'OPCODE', 'Parametro', 'MCODE', 'Parametro', 'MCODE', 'Parametro', 'MCODE'))
salida.write('{0:5s} {1:12s} \n'.format('OPCODE', 'Machine Code'))

for instruccion in tabla:
    if(len(tabla[instruccion])<3):
        TIPO, Param1 = tabla[instruccion]
        if (type(Param1) is int):
            Param1CODE = '{0:08b}'.format(Param1)
        else:
            Param1CODE = dicreg[Param1]
        string = '{0}{1:15s}{0} {0}{2:13s}{0} {0}{3:6s}{0} {0}{4:9s}{0} {0}{5:8s}{0}'.format('|', instruccion, TIPO, dicopcode[TIPO], str(Param1), Param1CODE)
        salida.write('{0:5s} {1:12s} \n'.format(dicopcode[TIPO], Param1CODE))
    elif(len(tabla[instruccion]) < 4):
        TIPO, Param1, Param2 = tabla[instruccion]
        if(type(Param1) is int):
            Param1CODE = '{0:08b}'.format(Param1)
        else:
            Param1CODE = dicreg[Param1]
        if (type(Param2) is int):
            Param2CODE = '{0:08b}'.format(Param2)
        else:
            Param2CODE = dicreg[Param2]
        string = '{0}{1:15s}{0} {0}{2:13s}{0} {0}{3:6s}{0} {0}{4:9s}{0} {0}{5:8s}{0} {0}{6:9s}{0} {0}{7:8s}{0}'.format('|', instruccion, TIPO, dicopcode[TIPO], str(Param1), Param1CODE, str(Param2), Param2CODE)
        salida.write('{0:5s} {1:4s} {2:8s} \n'.format(dicopcode[TIPO], Param1CODE, Param2CODE))
    else:
        TIPO, Param1, Param2, Param3 = tabla[instruccion]
        if (type(Param1) is int):
            Param1CODE = '{0:08b}'.format(Param1)
        else:
            Param1CODE = dicreg[Param1]
        if (type(Param2) is int):
            Param2CODE = '{0:08b}'.format(Param2)
        else:
            Param2CODE = dicreg[Param2]
        if (type(Param3) is int):
            Param3CODE = '{0:08b}'.format(Param3)
        else:
            Param3CODE = dicreg[Param3]
        string = '{0}{1:15s}{0} {0}{2:13s}{0} {0}{3:6s}{0} {0}{4:9s}{0} {0}{5:8s}{0} {0}{6:9s}{0} {0}{7:8s}{0} {0}{8:9s}{0} {0}{9:8s}{0}'.format('|', instruccion, TIPO, dicopcode[TIPO], str(Param1), Param1CODE, str(Param2), Param2CODE, str(Param3), Param3CODE)
        salida.write('{0:5s} {1:4s} {2:4s} {3:4s} \n'.format(dicopcode[TIPO], Param1CODE, Param2CODE, Param3CODE))

    print(string)

salida.close()