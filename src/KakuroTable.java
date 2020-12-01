
import com.company.PrologConection;

import javax.swing.*; // JFrame, JPanel, ...
import java.awt.*; // GridLayout
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;


public class KakuroTable extends JFrame {
    private JPanel MainPanel; // This is the window class
    public int sugerenciasDisponibles = 5;
    public String[][] originalGame;
    public String[][] currentGame;
    public String[][] solutionGame;
    public JPanel gamePanel = new JPanel();
    public JPanel buttons = new JPanel();
    public List<editableButton> buttonsList;
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

                }else {
                    if(pButton.getText().compareTo("") != 0){
                        currentGame[pButton.position][0] = Integer.toString(Integer.parseInt(pButton.getText())+1);
                        pButton.setText(Integer.toString(Integer.parseInt(pButton.getText())+1));

                    }else {
                        pButton.setText(Integer.toString(1));
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

    public KakuroTable(String[][] pTable, String[][] pSolution) {
        originalGame = pTable;
        currentGame = originalGame;
        solutionGame = pSolution;
        gamePanel.add(buttons);
        gamePanel.add(table);

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