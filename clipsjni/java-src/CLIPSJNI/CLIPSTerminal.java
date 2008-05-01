package CLIPSJNI;

import javax.swing.*; 
import javax.swing.border.*; 
import javax.swing.table.*;
import java.awt.*; 
import java.awt.event.*; 
 
import CLIPSJNI.*;

class CLIPSTerminal
  {  
   private JFrame jfrm;
         
   private Environment clips;
   
   private Thread executionThread;
      
   /***************/
   /* CLIPSTerminal */
   /***************/
   CLIPSTerminal()
     {  
      JTextAreaRouter jta;
      
      /*===================================*/
      /* Create a new JFrame container and */
      /* assign a layout manager to it.    */
      /*===================================*/
     
      jfrm = new JFrame("CLIPSTerminal");          
      jfrm.getContentPane().setLayout(new BoxLayout(jfrm.getContentPane(),BoxLayout.Y_AXIS));
    
      /*=================================*/
      /* Give the frame an initial size. */
      /*=================================*/
     
      jfrm.setSize(480,390);  
  
      /*=============================================================*/
      /* Terminate the program when the user closes the application. */
      /*=============================================================*/
     
      jfrm.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);  
 
      /*=============================*/
      /* Create the text field area. */
      /*=============================*/
 
      clips = new Environment();
      try
        { jta = new JTextAreaRouter(clips); }
      catch (Exception e)
        { 
         e.printStackTrace();
         return;
        }       
      clips.addRouter(jta);
      
      /*=======================================*/
      /* Put the text area into a scroll pane. */
      /*=======================================*/

      JScrollPane jscrlp = new JScrollPane(jta.getJTextArea());
      jscrlp.setPreferredSize(new Dimension(350,200));
      
      /*========================================*/
      /* Add the scroll pane to the main frame. */
      /*========================================*/
      
      jfrm.getContentPane().add(jscrlp); 
            
      /*====================*/
      /* Display the frame. */
      /*====================*/

      jfrm.pack();
      jfrm.setVisible(true);  
      
      /*======================================*/
      /* Start the CLIPS command loop thread. */
      /*======================================*/
      
      startCLIPS();
     }  
     
   /********/
   /* main */
   /********/  
   public static void main(String args[])
     {  
      /*===================================================*/
      /* Create the frame on the event dispatching thread. */
      /*===================================================*/
      
      SwingUtilities.invokeLater(
        new Runnable() 
          {  
           public void run() { new CLIPSTerminal(); }  
          });   
     }  
  
   /**************/
   /* startCLIPS */
   /**************/  
   public void startCLIPS()
     {
      Runnable runThread = 
         new Runnable()
           {
            public void run()
              { clips.commandLoop(); }
           };
      
      executionThread = new Thread(runThread);
      
      executionThread.start();
     }
  }