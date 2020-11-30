import org.jpl7.Query;
import org.jpl7.Term;

import java.util.Arrays;
import java.util.HashMap;
import java.util.StringTokenizer;

public class Main {

    public static void main(String[] args) {
        PrologConection prolog = new PrologConection();
        prolog.generarKakuro();
        String tableroKakuro[][] = prolog.getTableroKakuro();
        String tableroKakuroS[][] = prolog.getTableroKakuroSolucion();
        String listaKakuro[] = prolog.matrizAlista(tableroKakuro);
        String listaKakuroS[] = prolog.matrizAlista(tableroKakuroS);

        //Imprimir matriz
        System.out.println("TABLERO SIN SOLUCION");
        prolog.imprimirKakuro(tableroKakuro);
        System.out.println("TABLERO CON SOLUCION");
        prolog.imprimirKakuro(tableroKakuroS);

        //Convierte listas en strings, para enviarlas a prolog en verificar
        String strKakuro = prolog.listaAstring(listaKakuro);
        String strKakuroS = prolog.listaAstring(listaKakuroS);

        Integer[] verificaciones = prolog.verificarTablero(strKakuro, strKakuroS);
        System.out.println("Errores: " + verificaciones[0]);
        System.out.println("Casillas blancas: " + verificaciones[1]);
    }


}
