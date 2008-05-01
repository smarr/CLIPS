package CLIPSJNI;

import javax.swing.*; 
import javax.swing.border.*; 
import javax.swing.table.*;
import java.awt.*; 
import java.awt.event.*; 
import java.io.Reader;
import java.io.InputStreamReader;

public class JTextAreaRouter implements Router, KeyListener
  {  
   private Environment clips;
   private JTextArea jta;
   private StringBuffer buffer; 
   private boolean expectingInput;
   private int charInput;
   private long bufferInputCount;
   
   private Reader theReader;
   private boolean ungotten;
   private int lastChar;
   private int numChar;
   private int curChar;
   private int char2;
   private int char3;
      
   /*******************/
   /* JTextAreaRouter */
   /*******************/
   public JTextAreaRouter(
     Environment theEnv) throws Exception
     {  
      clips = theEnv;
      expectingInput = false;
      
      jta = new JTextArea();
      
      jta.setFont(new Font("Monospaced", Font.PLAIN, 12));
      jta.setEditable(false);
      jta.setMargin(new Insets(5,5,5,0));
      jta.addKeyListener(this);
      
      buffer = new StringBuffer(); 
      
      ungotten = false;
      numChar = 0;
      curChar = 0;
      theReader = new InputStreamReader(System.in,"UTF-8");
     }  
  
  /* Should following methods be public? */
     
   public JTextArea getJTextArea() 
     {
      return jta;
     }
     
   public void keyPressed(KeyEvent e) 
     {
      /* if (! expectingInput) return; */

      /* if (e.getID() != KeyEvent.KEY_TYPED) return; */
      
      /* if ((e.getModifiers() & (KeyEvent.ALT_MASK | KeyEvent.CTRL_MASK | KeyEvent.META_MASK)) != 0) return; */

      e.consume();
     }

   public void keyReleased(KeyEvent e) 
     { 
      /* if (! expectingInput) return; */

      /* if (e.getID() != KeyEvent.KEY_TYPED) return; */
      
      /* if ((e.getModifiers() & (KeyEvent.ALT_MASK | KeyEvent.CTRL_MASK | KeyEvent.META_MASK)) != 0) return; */

      e.consume();
     }

   public void keyTyped(KeyEvent e) 
     {
      synchronized (this)
        {
         if (! expectingInput) return;
      
         int id = e.getID();
      
         if (id != KeyEvent.KEY_TYPED) return;

         /* if ((e.getModifiers() & (KeyEvent.ALT_MASK | KeyEvent.CTRL_MASK | KeyEvent.META_MASK)) != 0) return; */
                  
         char c = e.getKeyChar();

         charInput = c;
         
         if (charInput == KeyEvent.VK_BACK_SPACE)
           {
            if (bufferInputCount <= 0) return;
            jta.append(buffer.toString());
            buffer = new StringBuffer();
            jta.replaceRange("",jta.getText().length() - 1,jta.getText().length());
           }
         else
           {
            buffer.append(c);    
            jta.append(buffer.toString());
            buffer = new StringBuffer();
           }
         
         e.consume();
         
         notify();
        }
     }

   /****************/
   /* getPriority: */
   /****************/
   public int getPriority()
     {
      return 10;
     }

   /************/
   /* getName: */
   /************/
   public String getName()
     {
      return "Router1Demo";
     }

   /**********/
   /* query: */
   /**********/
   public boolean query(
     String routerName)
     {      
      if (routerName.equals("stdout") ||
          routerName.equals("stdin") ||
          routerName.equals("wwarning") ||
          routerName.equals("werror") ||
          routerName.equals("wtrace") ||
          routerName.equals("wdialog") ||
          routerName.equals("wclips") ||
          routerName.equals("wdisplay"))
      
        { return true; }

      return false;
     }

   /**********/
   /* print: */
   /**********/
   public void print(
     String routerName,
     String printString)
     {
      bufferAppend(printString);
     }
  
   /*****************/
   /* bufferAppend: */
   /*****************/
   public synchronized void bufferAppend(
     String theString)
     {
      boolean invoke;

      if (buffer.length() != 0) 
        { invoke = false; }
      else
        { invoke = true; }
      
      buffer.append(theString); 

      if (! invoke) return;
      
      SwingUtilities.invokeLater(
         new Runnable()
           {
            public void run()
               {
                try 
                  { bufferUpdate(); }
                catch (Exception e)
                  { e.printStackTrace(); }
               }
           });
     }

   /*****************/
   /* bufferUpdate: */
   /*****************/
   public synchronized void bufferUpdate()
     {    
      jta.append(buffer.toString());
      buffer = new StringBuffer(); 
     }
     
   /************/
   /* getchar: */
   /************/
   public int getchar(
     String routerName)
     {
      int rv = -1;
      
      if (ungotten)
        {
         ungotten = false;
         return lastChar;
        }
        
      if (curChar < numChar)
        {
         if (curChar == 1)
           { rv = char2; }
         else
           { rv = char3; }
         curChar++;
         
         return rv;
        }

      synchronized(this)
        {
         expectingInput = true;
         bufferInputCount = clips.getInputBufferCount();
         try
           { 
            wait(); 
            
            if (charInput <= 127)
              { 
               numChar = 1;
               curChar = 1;
               return charInput; 
              }
            else if ((charInput > 127) && (charInput < 2048)) 
              {
               numChar = 2;
               curChar = 1;
               char2 = ((charInput & 0x3F) | 0x80);
               charInput = (((charInput >> 6) & 0x1F) | 0xC0);
               return charInput;
              }
            else
              {
               numChar = 3;
               curChar = 1;
               char2 = (((charInput >> 6) & 0x3F) | 0x80);
               char3 = ((charInput & 0x3F) | 0x80);
               charInput = (((charInput >> 12) & 0x0F) | 0xE0);
               return charInput;
              }
           }
         catch (Exception e)
           { e.printStackTrace(); }       
        }
      
      return -1;
     }

   /**************/
   /* ungetchar: */
   /**************/
   public int ungetchar(
     String routerName,
     int theChar)
     {
      if (ungotten)
        { return -1; }
        
      lastChar = theChar;
      ungotten = true;
      
      return theChar;
     }

   /*********/
   /* exit: */
   /*********/
   public boolean exit(
     int exitCode)
     {      
      return true;
     }
     
  }