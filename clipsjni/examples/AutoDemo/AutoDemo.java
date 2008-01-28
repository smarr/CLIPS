import javax.swing.*; 
import javax.swing.border.*; 
import javax.swing.table.*;
import java.awt.*; 
import java.awt.event.*; 
 
import java.util.Iterator;
import java.util.List;
 
import CLIPSJNI.*;

/* Implement FindFact which returns just a FactAddressValue or null */
/* TBD Add size method to PrimitiveValue */

class AutoDemo implements ActionListener
  {  
   JLabel displayLabel;
   JButton nextButton;
   JButton prevButton;
   JPanel choicesPanel;
   ButtonGroup choicesButtons;
 
   Environment clips;
      
   AutoDemo()
     {  
      /*================================*/
      /* Create a new JFrame container. */
      /*================================*/
     
      JFrame jfrm = new JFrame("Auto Demo");  
 
      /*=============================*/
      /* Specify FlowLayout manager. */
      /*=============================*/
        
      jfrm.getContentPane().setLayout(new GridLayout(3,1));  
 
      /*=================================*/
      /* Give the frame an initial size. */
      /*=================================*/
     
      jfrm.setSize(350,200);  
  
      /*=============================================================*/
      /* Terminate the program when the user closes the application. */
      /*=============================================================*/
     
      jfrm.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);  
 
      /*===========================*/
      /* Create the display panel. */
      /*===========================*/
      
      JPanel displayPanel = new JPanel(); 
      displayLabel = new JLabel("<html><br>");
      displayPanel.add(displayLabel);
      
      /*===========================*/
      /* Create the choices panel. */
      /*===========================*/
     
      choicesPanel = new JPanel(); 
      choicesButtons = new ButtonGroup();
      
      /*===========================*/
      /* Create the buttons panel. */
      /*===========================*/

      JPanel buttonPanel = new JPanel(); 
      
      prevButton = new JButton("Prev");
      prevButton.setActionCommand("Prev");
      buttonPanel.add(prevButton);
      prevButton.addActionListener(this);
      
      nextButton = new JButton("Next");
      nextButton.setActionCommand("Next");
      buttonPanel.add(nextButton);
      nextButton.addActionListener(this);
     
      /*=====================================*/
      /* Add the panels to the content pane. */
      /*=====================================*/
      
      jfrm.getContentPane().add(displayPanel); 
      jfrm.getContentPane().add(choicesPanel); 
      jfrm.getContentPane().add(buttonPanel); 

      /*========================*/
      /* Load the auto program. */
      /*========================*/
      
      clips = new Environment();
      
      clips.load("autodemo.clp");
      
      clips.reset();
      clips.run();
      nextUIState();
      
      /*====================*/
      /* Display the frame. */
      /*====================*/
      
      jfrm.setVisible(true);  
     }  

   /****************/
   /* nextUIState: */
   /****************/  
   private void nextUIState() 
     {
      /*=====================*/
      /* Get the state-list. */
      /*=====================*/
      
      String evalStr = "(find-all-facts ((?f state-list)) TRUE)";

      MultifieldValue pv = (MultifieldValue) clips.eval(evalStr);

      int tNum = pv.listValue().size();

      if (tNum == 0) return;

      FactAddressValue fv = (FactAddressValue) pv.listValue().get(0);
      
      String currentID = fv.getFactSlot("current").toString();

      /*===========================*/
      /* Get the current UI state. */
      /*===========================*/
      
      evalStr = "(find-all-facts ((?f UI-state)) " +
                                "(eq ?f:id " + currentID + "))";
      
      pv = (MultifieldValue) clips.eval(evalStr);
      
      tNum = pv.listValue().size();
      
      if (tNum == 0) return;
      
      fv = (FactAddressValue) pv.listValue().get(0);
      
      /*========================================*/
      /* Determine the Next/Prev button states. */
      /*========================================*/
      
      if (fv.getFactSlot("state").toString().equals("final"))
        { 
         nextButton.setActionCommand("Restart");
         nextButton.setText("Restart"); 
         prevButton.setVisible(true);
        }
      else if (fv.getFactSlot("state").toString().equals("initial"))
        {
         nextButton.setActionCommand("Next");
         nextButton.setText("Next");
         prevButton.setVisible(false);
        }
      else
        { 
         nextButton.setActionCommand("Next");
         nextButton.setText("Next");
         prevButton.setVisible(true);
        }
      
      /*=====================*/
      /* Set up the choices. */
      /*=====================*/
      
      choicesPanel.removeAll();
      choicesButtons = new ButtonGroup();
            
      pv = (MultifieldValue) fv.getFactSlot("valid-answers");
      
      List theList = pv.listValue();
      
      String selected = fv.getFactSlot("response").toString();
     
      for (Iterator itr = theList.iterator(); itr.hasNext();) 
        {
         PrimitiveValue bv = (PrimitiveValue) itr.next();
         JRadioButton rButton;
                                                            
         if (bv.toString().equals(selected))
            { rButton = new JRadioButton(bv.toString(),true); }
         else
            { rButton = new JRadioButton(bv.toString(),false); }
            
         rButton.setActionCommand(bv.toString());
         choicesPanel.add(rButton);
         choicesButtons.add(rButton);
        }
        
      choicesPanel.repaint();
      
      /*====================================*/
      /* Set the label to the display text. */
      /*====================================*/

      String theText = ((StringValue) fv.getFactSlot("display")).stringValue();
      
      displayLabel.setText("<html><br>" + theText);
     }

   /*########################*/
   /* ActionListener Methods */
   /*########################*/
   
   /*******************/
   /* actionPerformed */
   /*******************/  
   public void actionPerformed(
     ActionEvent ae) 
     { 
      /*=====================*/
      /* Get the state-list. */
      /*=====================*/
      
      String evalStr = "(find-all-facts ((?f state-list)) TRUE)";

      MultifieldValue pv = (MultifieldValue) clips.eval(evalStr);

      int tNum = pv.listValue().size();

      if (tNum == 0) return;

      FactAddressValue fv = (FactAddressValue) pv.listValue().get(0);
      
      String currentID = fv.getFactSlot("current").toString();

      /*=========================*/
      /* Handle the Next button. */
      /*=========================*/
      
      if (ae.getActionCommand().equals("Next"))
        {
         if (choicesButtons.getButtonCount() == 0)
           { clips.assertString("(next " + currentID + ")"); }
         else
           {
            clips.assertString("(next " + currentID + " " +
                               choicesButtons.getSelection().getActionCommand() + 
                               ")");
           }
           
         clips.run();
         nextUIState();
        }
      else if (ae.getActionCommand().equals("Restart"))
        { 
         clips.reset(); 
         clips.run();
         nextUIState();
        }
      else if (ae.getActionCommand().equals("Prev"))
        {
         clips.assertString("(prev " + currentID + ")");
         clips.run();
         nextUIState();
        }
     }
     
   public static void main(String args[])
     {  
      // Create the frame on the event dispatching thread.  
      SwingUtilities.invokeLater(
        new Runnable() 
          {  
           public void run() { new AutoDemo(); }  
          });   
     }  
  }