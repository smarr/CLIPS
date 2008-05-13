package CLIPSJNI;

import java.awt.Toolkit;
import javax.swing.*; 
import javax.swing.event.CaretEvent; 
import javax.swing.event.CaretListener; 
import java.awt.event.*; 

import java.util.List;
import java.util.ArrayList;

public class JTextAreaCommandPromptRouter extends JTextAreaRouter implements CaretListener
  { 
   private Thread executionThread;
   private boolean isExecuting = false;
   
   static final int DEFAULT_COMMAND_MAX = 25;
   
   int maxCommandCount;
   int currentCommandCount;
   int currentCommand;
   
   private int caretOffset;
   
   ArrayList<String> commandHistory;
   
   /********************************/
   /* JTextAreaCommandPromptRouter */
   /********************************/
   public JTextAreaCommandPromptRouter(
     Environment theEnv) throws Exception
     {  
      super(theEnv);
      
      theEnv.addRouter(this);
      theEnv.printBanner();
      theEnv.printPrompt();
      theEnv.setInputBufferCount(0);
      
      jta.getCaret().setVisible(true);
      jta.addCaretListener(this);
      
      maxCommandCount = DEFAULT_COMMAND_MAX;
      currentCommandCount = 1;
      currentCommand = 0;
      
      caretOffset = 0;
      
      commandHistory = new ArrayList<String>(DEFAULT_COMMAND_MAX); 
      commandHistory.add(new String(""));
     }  

   /**************/
   /* keyPressed */
   /**************/
   public void keyPressed(KeyEvent e) 
     {
      if (getExecuting())
        { 
         super.keyPressed(e); 
         return;
        }
 
      /* if (! expectingInput) return; */
      
      if (e.getID() != KeyEvent.KEY_PRESSED) return;
      
      if ((e.getModifiers() & (KeyEvent.ALT_MASK | KeyEvent.CTRL_MASK | KeyEvent.META_MASK)) != 0) return;

      int kc = e.getKeyCode();
      
      if ((kc == KeyEvent.VK_UP) || (kc == KeyEvent.VK_KP_UP))
        { 
         if ((currentCommand + 1) < commandHistory.size())
           {
            if ((e.getModifiers() & KeyEvent.SHIFT_MASK) != 0)
              { switchCommand(currentCommand,commandHistory.size() - 1); }
            else
              { switchCommand(currentCommand,currentCommand + 1); }
          }
        }
      else if ((kc == KeyEvent.VK_DOWN) || (kc == KeyEvent.VK_KP_DOWN))
        { 
         if (currentCommand != 0)
          {
           if ((e.getModifiers() & KeyEvent.SHIFT_MASK) != 0)
             { switchCommand(currentCommand,0); }
           else
             { switchCommand(currentCommand,currentCommand - 1); }
          }
        }
        
      /*============================*/
      /* Handle the left arrow key. */
      /*============================*/
      
      else if ((kc == KeyEvent.VK_LEFT) || (kc == KeyEvent.VK_KP_LEFT))
        { 
         String theCommand = clips.getInputBuffer();
         int commandSize = (int) clips.getInputBufferCount();

         /*=======================================================*/
         /* The caret can only be moved left if it is not already */
         /* at the beginning of the current command.              */
         /*=======================================================*/
         
         if (caretOffset < commandSize)
           {
            /*=============================================*/
            /* Move the caret back at least one character. */
            /*=============================================*/
            
            caretOffset++;
            
            /*=====================================================*/
            /* If the shift key is down, continue moving the caret */
            /* back until the beginning of the line is reached.    */
            /*=====================================================*/
            
            if ((e.getModifiers() & KeyEvent.SHIFT_MASK) != 0)
              { 
               while ((caretOffset < commandSize) &&
                      (theCommand.charAt(commandSize - (caretOffset + 1)) != '\n'))
                 { caretOffset++; }
              }
              
            /*=====================================*/
            /* Move the caret to its new position. */
            /*=====================================*/
            
            jta.getCaret().setDot(jta.getText().length() - caretOffset);
           }
        }
      else if ((kc == KeyEvent.VK_RIGHT) || (kc == KeyEvent.VK_KP_RIGHT))
        { 
         /*==================================================*/
         /* The caret can only be moved right if it is not   */
         /* already at the beginning of the current command. */
         /*==================================================*/

         if (caretOffset > 0)
           {
            String theCommand = clips.getInputBuffer();
            int commandSize = (int) clips.getInputBufferCount();

            /*================================================*/
            /* Move the caret forward at least one character. */
            /*================================================*/
            
            caretOffset--;
            
            /*=====================================================*/
            /* If the shift key is down, continue moving the caret */
            /* forward until the end of the line is reached.       */
            /*=====================================================*/

            if ((e.getModifiers() & KeyEvent.SHIFT_MASK) != 0)
              { 
               while ((caretOffset > 0) &&
                     (theCommand.charAt(commandSize - caretOffset) != '\n'))
                { caretOffset--; }
              }
              
            /*=====================================*/
            /* Move the caret to its new position. */
            /*=====================================*/
            
            jta.getCaret().setDot(jta.getText().length() - caretOffset);
           }
        }

      e.consume();
     }
     
   /************/
   /* keyTyped */
   /************/
   public void keyTyped(KeyEvent e) 
     {      
      if (getExecuting())
        { super.keyTyped(e); }
      else
        {
         int id = e.getID();
      
         if (id != KeyEvent.KEY_TYPED) return;

         if ((e.getModifiers() & (KeyEvent.ALT_MASK | KeyEvent.CTRL_MASK | KeyEvent.META_MASK)) != 0) return;
 
         moveSelectionToEnd();
         
         char c = e.getKeyChar();
         
         if (c == KeyEvent.VK_BACK_SPACE)
           {
            if (clips.getInputBufferCount() <= 0) return;
            jta.append(buffer.toString());
            buffer = new StringBuffer();
            jta.replaceRange("",jta.getText().length() - 1,jta.getText().length());
            expandInputBuffer(c);
            balanceParentheses();
           }
         else
           {
            buffer.append(c);    
            jta.append(buffer.toString());
            buffer = new StringBuffer();
            expandInputBuffer(c);
            balanceParentheses();
            commandCheck();
           }
                    
         e.consume();
        }
     }

   /**********************/
   /* balanceParentheses */
   /**********************/
   public void balanceParentheses() 
     {
      long commandLength;
   
      /*=======================================================*/
      /* Don't balance the parentheses if there is no command. */
      /*=======================================================*/
   
      commandLength = clips.getInputBufferCount();
      if (commandLength <= 0) 
        { return; }

      /*=================================*/
      /* Retrieve the current selection. */
      /*=================================*/
          
      int selStart = jta.getSelectionStart();
      int selEnd = jta.getSelectionEnd();

      /*======================*/
      /* Where is the cursor? */
      /*======================*/
    
      int cursorLocation = jta.getCaretPosition();
   
      if (cursorLocation == 0) return;
   
      cursorLocation--;
      
      /*===============================================*/
      /* What is the character at the cursor location? */
      /*===============================================*/
    
      char characterToCheck = jta.getText().charAt(cursorLocation);
      
      /*======================================*/
      /* We only balance a right parenthesis. */
      /*======================================*/
   
      if (characterToCheck != ')') return;

      /*======================================================================*/
      /* The nesting depth will start at zero. Each time a ')' is encountered */
      /* the nesting depth is incremented by one and each time a '(' is       */
      /* encountered the nesting depth is decremented by one. If a '(' is     */
      /* encountered when the nesting depth is zero (the starting value), the */
      /* matching parenthesis has been found.                                 */
      /*======================================================================*/
   
      int nestingDepth = 0;

      /*==================================================*/
      /* Start looking for the matching left parenthesis. */
      /*==================================================*/

      while ((cursorLocation-- != 0) && (commandLength-- != 0)) 
        {
         characterToCheck = jta.getText().charAt(cursorLocation);
         if (characterToCheck == '(') 
           {
            if (nestingDepth == 0) 
              {
               /*======================================*/
               /* Select the matching left parenthesis */
               /* and hide the carete.                 */
               /*======================================*/
               
               jta.getCaret().setVisible(false);
		     jta.setSelectionStart(cursorLocation);
		     jta.setSelectionEnd(cursorLocation + 1);

               /*========================================*/
               /* Force an update to occur otherwise the */
               /* changed selection won't be visible.    */
               /*========================================*/
               
               jta.update(jta.getGraphics());

               /*============================================*/
               /* Pause momentarily so the selected matching */
               /* parenthesis can be observed.               */
               /*============================================*/
               
		     try
		       { Thread.sleep(200); }
		     catch (Exception e)
		       { e.printStackTrace(); }

               /*===========================*/
               /* Restore the selection and */
               /* make the caret visible.   */
               /*===========================*/
               
		     jta.setSelectionStart(selStart);
		     jta.setSelectionEnd(selEnd);
		     jta.getCaret().setVisible(true);
		       
		     return;
		    }
            else
		   { nestingDepth--; }
	      }
         else if (characterToCheck == ')') 
           { nestingDepth++; }
        }

      /*================================================*/
      /* Beep to indicate a matching ')' was not found. */
      /*================================================*/
   
      Toolkit.getDefaultToolkit().beep();
     }

   /****************/
   /* commandCheck */
   /****************/
   public void commandCheck() 
     {
      if (clips.inputBufferContainsCommand())
        { 
         updateCommandHistory();
         executeCommand(); 
        }
     }

   /**********/
   /* print: */
   /**********/
   public void print(
     String routerName,
     String printString)
     {
      caretOffset = 0;

      super.print(routerName,printString);
     }

   /********/
   /* copy */
   /********/
   public void copy() 
     {
      jta.copy();
     }

   /*********/
   /* paste */
   /*********/
   public void paste() 
     {
      try
        {
         /*==================================*/
         /* Empty the buffer before pasting. */
         /*==================================*/
         
         bufferUpdate();
         
         /*======================================================*/
         /* Determine the text offset to the end before pasting. */
         /*======================================================*/
         
         int lineCount = jta.getLineCount();
         int end = jta.getLineEndOffset(lineCount-1);

         /*====================================================*/
         /* Set the selection to the end of the text area so   */
         /* that the paste is inserted at the end of the text. */
         /*====================================================*/
         
         moveSelectionToEnd();
         
         /*=================*/
         /* Paste the text. */
         /*=================*/
         
         jta.setEditable(true);
         jta.paste(); 
         jta.setEditable(false);
         jta.getCaret().setVisible(true);
            
         /*===========================*/
         /* Get the text just pasted. */
         /*===========================*/
            
         lineCount = jta.getLineCount();
         int newEnd = jta.getLineEndOffset(lineCount-1);
         String thePaste = jta.getText(end,newEnd - end);
         
         /*==============================================*/
         /* Append the paste to the CLIPS command buffer */
         /* and then check for a completed command.      */
         /*==============================================*/
         
         if (getExecuting())
           { setReadInputBuffer(thePaste); }
         else
           {
            clips.appendInputBuffer(thePaste);
            commandCheck();
           }
        }
     catch (Exception e)
        { e.printStackTrace(); }
     }

   /**********************/
   /* expandInputBuffer: */
   /**********************/
   public void expandInputBuffer(
     char theChar)
     {
      if (theChar <= 127)
        { clips.expandInputBuffer(theChar); }
      else if ((theChar > 127) && (theChar < 2048)) 
        {
         clips.expandInputBuffer((char) (((theChar >> 6) & 0x1F) | 0xC0));
         clips.expandInputBuffer((char) ((theChar & 0x3F) | 0x80));
        }
      else
        {
         clips.expandInputBuffer((char) (((theChar >> 12) & 0x0F) | 0xE0));
         clips.expandInputBuffer((char) (((theChar >> 6) & 0x3F) | 0x80));
         clips.expandInputBuffer((char) ((theChar & 0x3F) | 0x80));
        }
     }

   /**********************/
   /* moveSelectionToEnd */
   /**********************/
   private void moveSelectionToEnd() 
     {
      try
        {
         int lineCount = jta.getLineCount();
         int end = jta.getLineEndOffset(lineCount-1);
         jta.setSelectionStart(end);
         jta.setSelectionEnd(end);
        }
      catch (Exception e)
         { e.printStackTrace(); }
     }
        
   /****************/
   /* getExecuting */
   /****************/
   public synchronized boolean getExecuting() 
     {
      return isExecuting;
     }

   /****************/
   /* setExecuting */
   /****************/
   public synchronized void setExecuting(
     boolean value) 
     {
      isExecuting = value;
     }

   /************************/
   /* updateCommandHistory */
   /************************/  
   private void updateCommandHistory()
     {
      /*=================================================*/
      /* Replace the first command with the contents of  */
      /* the command string, up to but not including the */ 
      /* last carriage return which initiated execution  */
      /* of the command. Removing the last carriage      */
      /* will prevent the command from being immediately */
      /* executed when the command is recalled by the    */
      /* up/down arrow keys (i.e. the user must hit the  */
      /* final carriage return again to execute the      */
      /* recalled command).                              */
      /*=================================================*/

      String theCommand = clips.getInputBuffer();
      
      int length = theCommand.length();
      int i, lastCR;
   
      for (i = 0, lastCR = length; i < length; i++)
        {
         if (theCommand.charAt(i) == '\n')
           { lastCR = i; }
        }   

      commandHistory.set(0,theCommand.substring(0,lastCR));
      
      /*====================================================*/
      /* If this command is identical to the prior command, */
      /* don't add it to the command history.               */
      /*====================================================*/
    
      if ((commandHistory.size() > 1) &&
          (commandHistory.get(0).equals(commandHistory.get(1))))
        {
         commandHistory.set(0,new String(""));
         currentCommand = 0;
         return;
        }

      /*=================================================*/
      /* Add a new empty command to the top of the stack */
      /* in preparation for the next user command.       */
      /*=================================================*/

      commandHistory.add(0,new String(""));
      currentCommand = 0;
      currentCommandCount++;
            
      /*=============================================*/
      /* Remove commands at the end of the command   */
      /* history if the maximum number of remembered */
      /* commands is exceeded.                       */
      /*=============================================*/
   
      while (commandHistory.size() > maxCommandCount)
        {
         commandHistory.remove(maxCommandCount);
         currentCommandCount--;
        }
     }

   /*****************/
   /* switchCommand */
   /*****************/  
   private void switchCommand(
     int oldCommand,
     int newCommand)
     {
      /*=============================================*/
      /* Remove the current command from the window. */
      /*=============================================*/

      String theCommand = clips.getInputBuffer();
      
      int length = theCommand.length();
      
      jta.replaceRange("",jta.getText().length() - length,jta.getText().length());

      /*==============================================*/
      /* Replace the old command with the contents of */
      /* the command string, which will now include   */
      /* any edits the user made.                     */
      /*==============================================*/
      
      commandHistory.set(oldCommand,theCommand);
         
      /*======================*/
      /* Use the new command. */
      /*======================*/
   
      clips.setInputBuffer(commandHistory.get(newCommand));
      jta.append(commandHistory.get(newCommand));
      
      currentCommand = newCommand;
      
      caretOffset = 0;
     }

   /**********************/
   /* showCommandHistory */
   /**********************/  
   private void showCommandHistory()
     {
      System.out.println("------------");
      for (int i = 0; i < commandHistory.size(); i++)
        {
         System.out.println(i + " " + commandHistory.get(i));
        }
     }

   /******************/
   /* executeCommand */
   /******************/  
   public void executeCommand()
     {
      caretOffset = 0;
      
      setExecuting(true);
      
      Runnable runThread = 
         new Runnable()
           {
            public void run()
              { 
               clips.commandLoopOnceThenBatch(); 
               setExecuting(false);
              }
           };
      
      executionThread = new Thread(runThread);
      
      executionThread.start();
     }

   /*#######################*/
   /* CaretListener Methods */
   /*#######################*/

   /***************/
   /* caretUpdate */
   /***************/  
   public void caretUpdate(
     CaretEvent e) 
     {
      caretUpdateAction(e.getDot(), e.getMark(),this); 
     }
    
   /*********************/
   /* caretUpdateAction */
   /*********************/  
   protected void caretUpdateAction(
     final int dot,
     final int mark,
     final JTextAreaCommandPromptRouter theRouter) 
     {
      SwingUtilities.invokeLater(new Runnable() 
        {
         public void run()
           {
            synchronized(theRouter)
              {
               /*==============================================*/
               /* Attempting to move the caret outside of the  */
               /* text for the current command is not allowed. */
               /*==============================================*/
            
               if (dot == mark) 
                 { 
                  int tl = jta.getText().length();
                  int il = (int) clips.getInputBufferCount();
               
                  if (dot < (tl -il))
                    {
                     caretOffset = il;
                     jta.getCaret().setDot(tl-il);
                    }
                  else
                    { caretOffset = tl - dot; }

                  jta.getCaret().setVisible(true);
                 }
              
               /*======================================*/
               /* If text is selected, hide the caret. */
               /*======================================*/
            
               else
                 { jta.getCaret().setVisible(false); }
              }
           }
        });
     }    
  }