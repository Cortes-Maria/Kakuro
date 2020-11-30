import org.jpl7.Query;
import org.jpl7.Term;

import java.util.Arrays;
import java.util.HashMap;
import java.util.StringTokenizer;

public class PrologConection {

    String tableroKakuro[][];
    String tableroKakuroSolucion[][];

    public PrologConection() {
        this.tableroKakuro = new String[9][9];
        this.tableroKakuroSolucion = new String[9][9];
        crearConexion();
    }

    public void crearConexion(){
        /*Consulta al archivo de prolog, para acceder a su base de conocimientos*/
        String t1 = "consult('/home/randycj/Escritorio/Lenguajes/Kakuro/prolog/Proyecto03.pl')";
        Query q1 = new Query(t1);
        System.out.println(t1 + " " + (q1.hasSolution() ? "verdadero" : "fallo")); //mostrara mensaje  si hay o no conexion
    }

    public void generarKakuro(){
        /*
        Trae desde prolog un kakuro generado aleatoriamente junto con la solucion de este
        * E: N/A
        * S: N/A
        */
        String t2 = "generarKakuro(X, Y)";
        Query q2 = new Query(t2);
        java.util.HashMap solucion;
        solucion = (HashMap) q2.oneSolution();
        Term kakuroSolucion = (Term) solucion.get("X");
        Term kakuro = (Term) solucion.get("Y");

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

            this.tableroKakuroSolucion[i][j] = elemento1;
            this.tableroKakuro[i][j] = elemento2;
            kakuroSolucion = kakuroSolucion.arg(2);
            kakuro = kakuro.arg(2);

            j++;
        }
    }

    public String[] matrizAlista(String[][] kakuro) {
        /*
        Convierte una matriz en una lista, para que esta sea utilizada en prolog
        E: una matriz con el kakuro
        S: una lista con todos los elementos de la matriz
        */
        String listaKakuro[] = new String[81];
        for (int i=0; i<9; i++){
            for (int j=0; j<9; j++){
                listaKakuro[i*9+j] = kakuro[i][j];
            }
        }
        return listaKakuro;
    }

    public String listaAstring (String[] kakuro){
        /*
        Convierte una lista en un String, para que la pueda recibir prolog
        E: una lista
        S: un string que seria la lista
        */
        return Arrays.asList(kakuro).toString();
    }

    public void imprimirKakuro(String[][] kakuro){
        /*
        Imprime el kakuro en la terminal
        E: el kakuro
        S: N/A
        */
        for (int x=0; x < kakuro.length; x++) {
            System.out.print("|");
            for (int y=0; y < kakuro[x].length; y++) {
                System.out.print (kakuro[x][y]);
                if (y!=kakuro[x].length-1) System.out.print("\t");
            }
            System.out.println("|");
        }
    }

    public Integer[] verificarTablero(String kakuro, String kakuroSolucion) {
        /*
        Verifica el estado del kakuro, para esto lo hace desde prolog
        E: dos strings representando el kakuro que se esta solucionando, y este mismo con la solucion
        S: un array de enteros indicando la cantidad de errores y casillas en blanco de ese kakuro
        */
        String t3 = "verificarKakuro(" + kakuro + ", " + kakuroSolucion + ", X, Y)";
        Query q3 = new Query(t3);

        java.util.HashMap validacionKakuro;
        validacionKakuro = (HashMap) q3.oneSolution();
        int errores = Integer.parseInt(validacionKakuro.get("X").toString());
        int casillasBlancas = Integer.parseInt(validacionKakuro.get("Y").toString());

        Integer verificaciones[] = new Integer[2];
        verificaciones[0] = errores;
        verificaciones[1] = casillasBlancas;
        return verificaciones;
    }

    public String[][] getTableroKakuro() {
        return tableroKakuro;
    }

    public String[][] getTableroKakuroSolucion() {
        return tableroKakuroSolucion;
    }
}