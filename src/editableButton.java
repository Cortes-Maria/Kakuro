import com.company.PrologConection;

import javax.swing.*;
import java.awt.*;

public class editableButton extends JButton {
    public int position;
    public editableButton(String label, int pPosition) {
        super(label);
        position = pPosition;
    }
    public static void main (String[] args) {
        editableButton button = new editableButton("",5);

        JFrame frame = new JFrame();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(400, 400);

        Container contentPane = frame.getContentPane();
        contentPane.setLayout(new FlowLayout());
        contentPane.add(button);

        frame.setVisible(true);
    }
}
