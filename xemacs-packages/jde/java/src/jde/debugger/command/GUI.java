package jde.debugger.gui;

import java.awt.GridLayout;
import javax.swing.JFrame;
import javax.swing.JPanel;

import jde.debugger.Debugger;
import jde.debugger.JDE;
import jde.debugger.Protocol;


/**
 * This is a first shot at trying to produce a GUI that displays debugging info from
 * JDEbug. I recommend a redesign, either after studying this a bit more, or straight
 * away, depending on the experience of the developer. The main things I am unhappy with
 * are:
 * <ul>
 *
 *   <li>it feels unelegant to have the commands (GetLocals, etc) be
 *   aware of who is the recipient of their results. A better design
 *   would be to let the Debugger decide where to send certain events,
 *   command results, etc.</li>
 *
 *   <li>this class itself is a hack - for instance, there's been very
 *   little thought on member variables are actually necessary, what
 *   JTree-related classes to use, and so on. I've been using it to
 *   learn more about JTree, but wasn't able to finish.</li>
 *
 *   <li>there is (maybe was, now) a bug in the AWT threads, meaning
 *   that it hasn't been possible to shut them down in any other way
 *   than with a System.exit() command.  The Quit command does that
 *   now, which is bad since it can hide other problems. Also, the
 *   whole shutdown sequence is a little messy, maybe due to an
 *   unclear division of labour between the SessionManager, Debugger
 *   and command classes.</li>
 *
 * </ul>
 *
 * <p>
 * Created: Thu Jan 31 13:13:39 2002
 *
 * @author <a href="mailto:petter.mahlen@chello.se">Petter Måhlén</a>
 * @author <a href="mailto:udalrich@carolingia.org">Troy Daniels</a>
 * @version
 */

public class GUI implements Protocol {
    private JFrame   m_frame;

    public GUI(final Debugger debugger) {
	m_frame    = new JFrame("JDEbug " + debugger.getProcID());
	m_frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);

	// Set up the GUI stuff
	JPanel basePanel = new JPanel();
	basePanel.setLayout(new GridLayout(0, 1));
	basePanel.add(new LocalVariableDisplay(debugger));


	// Finally, add the base panel to the content pane of the frame.
	m_frame.getContentPane().add(basePanel);
	m_frame.pack();
	m_frame.show();

	JDE.debug(FRAMEWORK, "GUI constructor done");
    }

  /** Shutdown the GUI */
  public synchronized void shutdown() {
    if (null != m_frame)
      m_frame.dispose();
  }
  protected void finalize() throws Throwable {
    super.finalize();
    shutdown();
  }


}// GUI
