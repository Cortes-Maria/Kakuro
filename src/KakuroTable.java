
import com.company.PrologConection;

import javax.swing.*; // JFrame, JPanel, ...
import java.awt.*; // GridLayout
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;


public class KakuroTable extends JFrame {
    private JPanel MainPanel; // This is the window class

    private JPanel gamePanel = new JPanel();
    private JPanel buttons = new JPanel();
    private JPanel table = new JPanel();
    private JButton generar = new JButton("Generar");
    private JButton validar = new JButton("Validar");
    private JButton reiniciar = new JButton("Reiniciar");

    public void changeNumber(JButton pButton) {
        pButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                //your actions
                if(pButton.getText().compareTo("9") == 0){
                    pButton.setText("");

                }else {
                    if(pButton.getText().compareTo("") != 0){
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
            }
        }
    }

    public KakuroTable(String[][] pTable) {
        gamePanel.add(buttons);
        gamePanel.add(table);

        this.add(gamePanel);
        this.setSize(800,500);

        buttonsGenerator();
        tableGenerator(pTable);

    }
    public static void main (String[] args) {
        //String[][] ejemplo = {{"0"},{"0"},{"x","1"},{"0"},{"0"},{"0"},{"3","6"},{"0"},{"0"},{"0"},{"44","3"},{"0"},{"0"},{"0"},{"0"},{"x"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"},{"0"}};
        com.company.PrologConection prolog = new PrologConection();
        prolog.generarKakuro();
        String[][] kakuro = prolog.getMatrizKak();
        JFrame frame = new KakuroTable(kakuro);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE); // 3
        frame.setExtendedState(JFrame.MAXIMIZED_BOTH); // 6
        frame.setVisible(true);
    }
}