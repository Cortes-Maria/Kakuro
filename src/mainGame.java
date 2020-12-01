
//no se si esta bien ese import

import com.company.PrologConection;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Random;

public class mainGame {
    PrologConection prolog;
    String tableroKakuro[][];
    String tableroKakuroS[][];
    KakuroTable table;

    public Integer[] solicitarSugerencia(String[] kakuro, String[] kakuroResuelto){
        Random r = new Random();
        int casilla = r.nextInt(81);
        while (true){

            if (kakuro[casilla].length() > 1 || kakuro[casilla].equals("x")){
                casilla = r.nextInt(81);
                continue;
            }

            if (kakuro[casilla].equals(kakuroResuelto[casilla])){
                casilla = r.nextInt(81);
                continue;
            }
            break;
        }

        Integer[] sugerencia = new Integer[2];

        sugerencia[0] = casilla;
        sugerencia[1] = Integer.parseInt(kakuroResuelto[casilla]);

        return sugerencia;

    }
 /*
 aqui va la accion de generar el kakuro
  */
    public void generarKakuro(JButton pButton) {
        pButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                 table.generarTable();
            }
        });
    }

    /*
    aqui va la accion de validar el kakuro
     */
    public void validarKakuro(JButton pButton) {
        pButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                String[] listaKakuro = prolog.matrizAlista(table.currentGame);
                String[] listaKakuroS = prolog.matrizAlista(tableroKakuroS);
                Integer[] validacion = prolog.verificarTablero(prolog.listaAstring(listaKakuro), prolog.listaAstring(listaKakuroS));
                table.validacionesGenerator(validacion);
            }
        });
    }
    /*
    aqui va la accion de reiniciar el kakuro
    */
    public void reiniciarKakuro(JButton pButton) {
        pButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                table.reiniciarTable();
            }
        });
    }
    /*
    aqui va la accion de mostrar la solucion del kakuro
    */
    public void mostrarSolucionKakuro(JButton pButton) {
        pButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                table.updateTable(tableroKakuroS);
            }
        });
    }
    /*
     /*
     aqui va la accion de solicitar sugerencia del kakuro
     */
    public void sugerenciaKakuro(JButton pButton) {
        pButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Integer[] sugerencia = solicitarSugerencia(prolog.matrizAlista(table.currentGame), prolog.matrizAlista(tableroKakuroS));
                for (int i = 0; i < table.buttonsList.size(); i++) {
                    if (table.buttonsList.get(i).position == sugerencia[0]) {
                        if(table.sugerenciasDisponibles > 0){
                            table.buttonsList.get(i).setText(Integer.toString(sugerencia[1]));//setea el boton con el numero sugerido
                            table.currentGame[table.buttonsList.get(i).position][0] = Integer.toString(sugerencia[1]);
                            table.sugerencias.setText("Sugerencias (" + Integer.toString(table.sugerenciasDisponibles - 1) + ")");//resta una sugerencia disponible
                            table.sugerenciasDisponibles--;
                        }

                    }
                }
            }
        });
    }



    public mainGame() {
        prolog = new PrologConection();
        prolog.generarKakuro();
        tableroKakuro = prolog.getMatrizKak();
        tableroKakuroS = prolog.getMatrizKakSolucion();

        JFrame frame = new KakuroTable(tableroKakuro,tableroKakuroS);
        table = (KakuroTable) frame;
        generarKakuro(table.generar);
        validarKakuro(table.validar);
        reiniciarKakuro(table.reiniciar);
        mostrarSolucionKakuro(table.mostrarSolucion);
        sugerenciaKakuro(table.sugerencias);
        table.tableGenerator(tableroKakuro);



        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE); // 3
        frame.setExtendedState(JFrame.MAXIMIZED_BOTH); // 6
        frame.setVisible(true);

    }
    /*Test*/
    public static void main(String[] args) {
       mainGame juego = new mainGame();
    }
}