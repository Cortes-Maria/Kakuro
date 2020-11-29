import org.jpl7.Query;
import org.jpl7.Term;

import java.util.Arrays;
import java.util.HashMap;
import java.util.StringTokenizer;

public class Main {

    public static void main(String[] args) {
        //Siempre tiene que hacer esta consulta
        String t1 = "consult('/home/randycj/Escritorio/Lenguajes/Kakuro/prolog/Proyecto03.pl')";
        Query q1 = new Query(t1);
        System.out.println(t1 + " " + (q1.hasSolution() ? "verdadero" : "fallo")); //mostrara mensaje  si hay o no conexion

        //Esto es para generar el kakuro aleatorio
        String t2 = "generarKakuro(X, Y)";
        Query q2 = new Query(t2);
        java.util.HashMap solucion;
        solucion = (HashMap) q2.oneSolution();
        Term kakuroSolucion = (Term) solucion.get("X");
        Term kakuro = (Term) solucion.get("Y");

        //las ultimas dos listas son para enviarlas de vuelta a prolog para verificar
        String tableroKakuroS[][] = new String[9][9];//matriz con solucion
        String tableroKakuro[][] = new String[9][9];//matriz sin solucion
        String listaKakuroS[] = new String[81];//lista con solucion
        String listaKakuro[] = new String[81];//lista sin solucion

        //se encarga de rellenar las matrices y listas con los kakuros generados aleatoriamente
        int i = 0;
        int j = 0;
        while(kakuroSolucion.arity() == 2) {
            if (j == 9) {
                i++;
                j = 0;
            }
            String elemento1 = kakuroSolucion.arg(1).toString();
            String elemento2 = kakuro.arg(1).toString();

            if (elemento1.substring(0, 1).equals("'")) {
                elemento1 = elemento1.substring(4, elemento1.length()-1);
                StringTokenizer tokens = new StringTokenizer(elemento1, ",");
                String str1 = tokens.nextToken();
                String str2 = tokens.nextToken().substring(1);
                elemento1 = str1 + "/" + str2;
                elemento2 = elemento1;
            }

            tableroKakuroS[i][j] = elemento1;
            tableroKakuro[i][j] = elemento2;
            listaKakuroS[i*9+j] = elemento1;
            listaKakuro[i*9+j] = elemento2;
            kakuroSolucion = kakuroSolucion.arg(2);
            kakuro = kakuro.arg(2);

            j++;
        }

        //Imprimir matriz
        System.out.println("TABLERO SIN SOLUCION");
        for (int x=0; x < tableroKakuro.length; x++) {
            System.out.print("|");
            for (int y=0; y < tableroKakuro[x].length; y++) {
                System.out.print (tableroKakuro[x][y]);
                if (y!=tableroKakuro[x].length-1) System.out.print("\t");
            }
            System.out.println("|");
        }

        System.out.println("TABLERO CON SOLUCION");
        for (int x=0; x < tableroKakuroS.length; x++) {
            System.out.print("|");
            for (int y=0; y < tableroKakuroS[x].length; y++) {
                System.out.print (tableroKakuroS[x][y]);
                if (y!=tableroKakuroS[x].length-1) System.out.print("\t");
            }
            System.out.println("|");
        }

        //Convierte listas en strings, para enviarlas a prolog en verificar
        String strKakuroS = Arrays.asList(listaKakuroS).toString();
        String strKakuro = Arrays.asList(listaKakuro).toString();
        //System.out.println(strKakuroS);
        //System.out.println(strKakuro);

        //se llama a prolog para verificar el kakuro que se este jugando
        //aqui se deberia convertir la matriz en funcion y luego es string
        //strKakuro es el kakuro que se esta solucionando, strKakuroS tiene la solucion
        String t3 = "verificarKakuro(" + strKakuro + ", " + strKakuroS + ", X, Y)";
        Query q3 = new Query(t3);

        //obtiene los errores y casillas blancas del juego que se le mande
        java.util.HashMap validacionKakuro;
        validacionKakuro = (HashMap) q3.oneSolution();
        String errores = validacionKakuro.get("X").toString();
        String casillasBlancas = validacionKakuro.get("Y").toString();

        System.out.println("Casillas sin rellenar: " + casillasBlancas);
        System.out.println("Total de errores: " + errores);

    }
}
