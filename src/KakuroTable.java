import javax.swing.*; // JFrame, JPanel, ...
import java.awt.*; // GridLayout

public class KakuroTable extends JFrame {
    private JPanel MainPanel; // This is the window class

    public KakuroTable() {
        this.setLayout(new GridLayout(9, 9)); // This makes the frame into a 9 x 9 grid
        this.setSize(300,300);
        for (int i = 0; i < 81; i++) {
            JButton button=new JButton(Integer.toString(i));
            this.add(button);
        }
    }
    public static void main (String[] args) {
        JFrame frame = new KakuroTable();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE); // 3
        frame.setExtendedState(JFrame.MAXIMIZED_BOTH); // 6
        frame.setVisible(true);
    }
}