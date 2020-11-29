import javax.swing.*; // JFrame, JPanel, ...
import java.awt.*; // GridLayout
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;


public class KakuroTable extends JFrame {
    private JPanel MainPanel; // This is the window class

    public void changeNumber(JButton pButton) {
        pButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                //your actions
                if(pButton.getText().compareTo("9") == 0){
                    pButton.setText("0");
                }else {
                    pButton.setText(Integer.toString(Integer.parseInt(pButton.getText())+1));
                }
            }
        });
    }

    public KakuroTable(/*String[] kakuro*/) {
        this.setLayout(new GridLayout(9, 9)); // This makes the frame into a 9 x 9 grid
        this.setSize(500,500);
        for (int i = 0; i < 81; i++) {
            JButton button = new JButton("0");
            changeNumber(button);
            //JButton button=new JButton(Integer.toString(i));
            //int[] numbers = {22,11};
            //LinedButton button = new LinedButton(numbers);
            this.add(button);
        }
    }
    public static void main (String[] args) {
        JFrame frame = new KakuroTable();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE); // 3
        //frame.setExtendedState(JFrame.MAXIMIZED_BOTH); // 6
        frame.setVisible(true);
    }
}