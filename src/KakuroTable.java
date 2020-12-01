
import com.company.PrologConection;

import javax.swing.*; // JFrame, JPanel, ...
import java.awt.*; // GridLayout
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;


public class KakuroTable extends JFrame {
    private JPanel MainPanel; // This is the window class
    public int sugerenciasDisponibles = 5;
    public String[][] originalGame;
    public String[][] currentGame;
    public String[][] solutionGame;
    public JPanel gamePanel = new JPanel();
    public JPanel buttons = new JPanel();

    public JPanel validaciones = new JPanel();
    public JLabel errores = new JLabel();
    public JLabel pendientes = new JLabel();
    public JLabel ganador = new JLabel();

    public List<editableButton> buttonsList = new ArrayList<editableButton>();
    public JPanel table = new JPanel();
    public JButton generar = new JButton("Generar");
    public JButton validar = new JButton("Validar");
    public JButton reiniciar = new JButton("Reiniciar");
    public JButton mostrarSolucion = new JButton("mostrarSolucion");
    public JButton sugerencias = new JButton("Sugerencias (" + Integer.toString(sugerenciasDisponibles)+")");

    public void changeNumber(editableButton pButton) {
        pButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                //your actions
                if(pButton.getText().compareTo("9") == 0){
                    pButton.setText("");
                    currentGame[pButton.position][0] = "0";

                }else {
                    if(pButton.getText().compareTo("") != 0){
                        currentGame[pButton.position][0] = Integer.toString(Integer.parseInt(pButton.getText())+1);
                        pButton.setText(Integer.toString(Integer.parseInt(pButton.getText())+1));

                    }
                    else {
                        pButton.setText(Integer.toString(1));
                        currentGame[pButton.position][0] = "1";
                    }
                }
            }
        });
    }
    public void buttonsGenerator() {
        buttons.setSize(200,500);
        buttons.add(generar);
        buttons.add(validar);
        buttons.add(reiniciar);
        buttons.add(mostrarSolucion);
        buttons.add(sugerencias);
    }
    public void validacionesGenerator(Integer[] validations) {
        validaciones.setSize(800,500);

        errores.setText("Cantidad de errores: "+Integer.toString(validations[0]));
        pendientes.setText("Cantidad de pendientes: " + Integer.toString(validations[1]));

        if(validations[0] == 0 && validations[1] == 0){
            ganador.setText("Felicidades, ha ganado!");
        }else {
            ganador.setText("Aun no ha ganado");
        }

        validaciones.add(errores);
        validaciones.add(pendientes);
        validaciones.add(ganador);


    }
    public void tableGenerator(String[][] pTable) {
        table.setLayout(new GridLayout(9, 9)); // This makes the frame into a 9 x 9 grid
        table.setSize(500,500);
        for (int i = 0; i < 81; i++) {
            if(pTable[i][1] != null || pTable[i][0].equals("x")) {
                LinedButton button = new LinedButton(pTable[i]);
                table.add(button);
            }else {
                editableButton button = new editableButton("",i);
                changeNumber(button);
                table.add(button);
                buttonsList.add(button);
            }
        }
    }

    public void updateTable(String[][] pTable) {
        for (int i = 0; i < 81; i++) {
            if(pTable[i][1] != null || pTable[i][0].equals("x")) {

            }else {
                buttonsList.get(getButtonIndex(i)).setText(pTable[i][0]);
            }
        }
    }
    public int getButtonIndex(int iPosition){
        for (int i = 0; i < buttonsList.size(); i++) {
            if (buttonsList.get(i).position == iPosition) {
                return i;
            }
        }
        return 0;
    }

    public KakuroTable(String[][] pTable, String[][] pSolution) {
        originalGame = pTable;
        currentGame = originalGame;
        solutionGame = pSolution;
        gamePanel.add(buttons);
        gamePanel.add(table);
        gamePanel.add(validaciones);

        this.add(gamePanel);
        this.setSize(800,500);

        buttonsGenerator();

    }
    public static void main (String[] args) {
        //String[][] ejemplo = {{"0"},{"0"},{"x","1"},{"0"},{"0"},{"0"},{"3","6"},{"0"},{"0"},{"0"},{"44","3"},{"0"},{"0"},{"0"},{"0"},{"x"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"}};
        com.company.PrologConection prolog = new PrologConection();
        prolog.generarKakuro();
        String[][] kakuro = prolog.getMatrizKak();
        String tableroKakuroS[][] = prolog.getMatrizKakSolucion();
        JFrame frame = new KakuroTable(kakuro,tableroKakuroS);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE); // 3
        frame.setExtendedState(JFrame.MAXIMIZED_BOTH); // 6
        frame.setVisible(true);
    }
}