import java.awt.*;
import java.lang.reflect.Array;

import javax.swing.*;
import javax.swing.border.Border;

public class LinedButton extends JLabel {

    private static final long serialVersionUID = 1L;

    public LinedButton(int[] numbers) {
        this.setText(" "+ Integer.toString(numbers[0])+"     "+Integer.toString(numbers[1]));

        Border border = BorderFactory.createLineBorder(Color.BLACK, 1);

        // set the border of this component
        this.setBorder(border);
        this.setBackground(Color.GRAY);
        this.setOpaque(true);
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);

        int x1 = 0;
        int y1 = 0;
        int x2 = this.getWidth();
        int y2 = this.getHeight();

        // and then draw it
        g.drawLine(x1, y1, x2, y2);


    }

    @Override
    public Dimension getPreferredSize() {
        Dimension size = super.getPreferredSize();
        size.width += size.height;
        return size;
    }

    /*Test the button*/
    public static void main(String[] args) {
        //LinedButton button = new LinedButton();

        JFrame frame = new JFrame();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(400, 400);

        Container contentPane = frame.getContentPane();
        contentPane.setLayout(new FlowLayout());
        //contentPane.add(button);

        frame.setVisible(true);
    }

}