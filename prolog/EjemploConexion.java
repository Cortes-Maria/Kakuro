
public class Main {

    public static void main(String[] args) {
        PrologConection prolog = new PrologConection();
        prolog.generarKakuro();
        String tableroKakuro[][] = prolog.getMatrizKak();
        String tableroKakuroS[][] = prolog.getMatrizKakSolucion();

        for (int i = 0; i < 81; i++) {
            if(tableroKakuro[i][1] != null || tableroKakuro[i][0].equals("x")) {
                System.out.print("Cuadro negro ");
                System.out.print(tableroKakuro[i][0]);
                if (tableroKakuro[i][1] != null) {
                    System.out.print("/" + tableroKakuro[i][1]);
                }
                System.out.println("");
            }else {
                System.out.print("Cuadro blanco");
                System.out.println(tableroKakuro[i][0]);
            }
        }

        String listaKakuro[] = prolog.matrizAlista(tableroKakuro);
        String listaKakuroS[] = prolog.matrizAlista(tableroKakuroS);

        //Convierte listas en strings, para enviarlas a prolog en verificar
        String strKakuro = prolog.listaAstring(listaKakuro);
        String strKakuroS = prolog.listaAstring(listaKakuroS);

        Integer[] verificaciones = prolog.verificarTablero(strKakuro, strKakuroS);
        System.out.println("Errores: " + verificaciones[0]);
        System.out.println("Casillas blancas: " + verificaciones[1]);
    }


}
