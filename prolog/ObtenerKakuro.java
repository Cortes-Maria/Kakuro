import org.jpl7.Query;
import org.jpl7.Term;
import java.util.HashMap;

public class Main {

    public static void main(String[] args) {
        String t1 = "consult('/home/randycj/Escritorio/Lenguajes/Kakuro/prolog/Proyecto03.pl')";
        Query q1 = new Query(t1);
        System.out.println(t1 + " " + (q1.hasSolution() ? "verdadero" : "fallo")); //mostrara mensaje  si hay o no conexion

        String t2 = "generarKakuro(X, Y)";
        Query q2 = new Query(t2);
        java.util.HashMap solucion;
        solucion = (HashMap) q2.oneSolution();
        Term kakuroSolucion = (Term) solucion.get("X");
        Term kakuro = (Term) solucion.get("Y");

        String tableroKakuroS[][] = new String[9][9];
        String tableroKakuro[][] = new String[9][9];

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
            }
            if (elemento2.substring(0, 1).equals("'")) {
                elemento2 = elemento2.substring(4, elemento2.length()-1);
            }
            tableroKakuroS[i][j] = elemento1;
            tableroKakuro[i][j] = elemento2;
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

    }
}
