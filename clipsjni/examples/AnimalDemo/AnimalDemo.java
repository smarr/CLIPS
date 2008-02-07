import javax.swing.*; 
import javax.swing.border.*; 
import javax.swing.table.*;
import java.awt.*; 
import java.awt.event.*; 
 
import java.util.Iterator;
import java.util.List;
 
import java.text.BreakIterator;

import java.util.Locale;
import java.util.ResourceBundle;
import java.util.MissingResourceException;

import CLIPSJNI.*;

/* Implement FindFact which returns just a FactAddressValue or null */
/* TBD Add size method to PrimitiveValue */

class AnimalDemo implements ActionListener
  {  
   JLabel displayLabel;
   JButton nextButton;
   JButton prevButton;
   JPanel choicesPanel;
   ButtonGroup choicesButtons;
   ResourceBundle animalResources;

   Environment clips;
      
   AnimalDemo()
     {  
      try
        {
         animalResources = ResourceBundle.getBundle("resources.AnimalResources",Locale.getDefault());
        }
      catch (MissingResourceException mre)
        {
         mre.printStackTrace();
         return;
        }

      /*================================*/
      /* Create a new JFrame container. */
      /*================================*/
     
      JFrame jfrm = new JFrame(animalResources.getString("AnimalDemo"));  
 
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
      displayLabel = new JLabel(); 
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
      
      prevButton = new JButton(animalResources.getString("Prev"));
      prevButton.setActionCommand("Prev");
      buttonPanel.add(prevButton);
      prevButton.addActionListener(this);
      
      nextButton = new JButton(animalResources.getString("Next"));
      nextButton.setActionCommand("Next");
      buttonPanel.add(nextButton);
      nextButton.addActionListener(this);
     
      /*=====================================*/
      /* Add the panels to the content pane. */
      /*=====================================*/
      
      jfrm.getContentPane().add(displayPanel); 
      jfrm.getContentPane().add(choicesPanel); 
      jfrm.getContentPane().add(buttonPanel); 

      /*==========================*/
      /* Load the animal program. */
      /*==========================*/
      
      clips = new Environment();
      
      clips.load("bcdemo.clp");
      clips.load("animaldemo.clp");
      clips.reset();
      clips.run();

      /*====================*/
      /* Display the frame. */
      /*====================*/
      
      jfrm.setVisible(true);  

      /*==================================*/
      /* Get the current state of the UI. */
      /*==================================*/
      
      nextUIState();
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
         nextButton.setText(animalResources.getString("Restart")); 
         prevButton.setVisible(true);
        }
      else if (fv.getFactSlot("state").toString().equals("initial"))
        {
         nextButton.setActionCommand("Next");
         nextButton.setText(animalResources.getString("Next"));
         prevButton.setVisible(false);
        }
      else
        { 
         nextButton.setActionCommand("Next");
         nextButton.setText(animalResources.getString("Next"));
         prevButton.setVisible(true);
        }
      
      /*=====================*/
      /* Set up the choices. */
      /*=====================*/
      
      choicesPanel.removeAll();
      choicesButtons = new ButtonGroup();
            
      pv = (MultifieldValue) fv.getFactSlot("valid-answers");      
      List theList1 = pv.listValue();
      
      pv = (MultifieldValue) fv.getFactSlot("display-answers");
      List theList2 = pv.listValue();

      String selected = fv.getFactSlot("response").toString();
     
      Iterator itr1, itr2;
      
      for (itr1 = theList1.iterator(), itr2 = theList2.iterator(); 
           itr1.hasNext() && itr2.hasNext();) 
        {
         PrimitiveValue bv1 = (PrimitiveValue) itr1.next();
         PrimitiveValue bv2 = (PrimitiveValue) itr2.next();
         JRadioButton rButton;
                                                            
         if (bv1.toString().equals(selected))
            { rButton = new JRadioButton(animalResources.getString(bv2.getValue().toString()),true); }
         else
            { rButton = new JRadioButton(animalResources.getString(bv2.getValue().toString()),false); }
            
         rButton.setActionCommand(bv1.toString());
         choicesPanel.add(rButton);
         choicesButtons.add(rButton);
        }
        
      choicesPanel.repaint();
      
      /*====================================*/
      /* Set the label to the display text. */
      /*====================================*/

      String theText = animalResources.getString(((SymbolValue) fv.getFactSlot("display")).stringValue());
 
      wrapLabelText(displayLabel,theText);
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
           { clips.assertString("(next (id " + currentID + ") (value-set FALSE))"); }
         else
           {
            clips.assertString("(next (id " + currentID + ") (value " +
                               choicesButtons.getSelection().getActionCommand() + 
                               ") (value-set TRUE))");
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
         clips.assertString("(prev (id " + currentID + "))");
         clips.run();
         nextUIState();
        }
     }
     
   /*****************/
   /* wrapLabelText */
   /*****************/  
   private void wrapLabelText(
     JLabel label, 
     String text) 
     {
      FontMetrics fm = label.getFontMetrics(label.getFont());
      Container container = label.getParent();
      int containerWidth = container.getWidth();
      int textWidth = SwingUtilities.computeStringWidth(fm,text);
      int desiredWidth;

      if (textWidth <= containerWidth)
        { desiredWidth = containerWidth; }
      else
        { 
         int lines = (int) ((textWidth + containerWidth) / containerWidth);
                  
         desiredWidth = (int) (textWidth / lines);
        }
                 
      BreakIterator boundary = BreakIterator.getWordInstance();
      boundary.setText(text);
   
      StringBuffer trial = new StringBuffer();
      StringBuffer real = new StringBuffer("<html><center>");
   
      int start = boundary.first();
      for (int end = boundary.next(); end != BreakIterator.DONE;
           start = end, end = boundary.next())
        {
         String word = text.substring(start,end);
         trial.append(word);
         int trialWidth = SwingUtilities.computeStringWidth(fm,trial.toString());
         if (trialWidth > containerWidth) 
           {
            trial = new StringBuffer(word);
            real.append("<br>");
            real.append(word);
           }
         else if (trialWidth > desiredWidth)
           {
            trial = new StringBuffer("");
            real.append(word);
            real.append("<br>");
           }
         else
           { real.append(word); }
        }
   
      real.append("</html>");
   
      label.setText(real.toString());
     }

   public static void main(String args[])
     {  
      // Create the frame on the event dispatching thread.  
      SwingUtilities.invokeLater(
        new Runnable() 
          {  
           public void run() { new AnimalDemo(); }  
          });   
     }  
  }