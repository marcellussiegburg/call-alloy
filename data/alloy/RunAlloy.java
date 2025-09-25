package alloy;

import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.ast.Module;
import edu.mit.csail.sdg.parser.CompUtil;
import edu.mit.csail.sdg.translator.A4Options;
import edu.mit.csail.sdg.translator.A4Solution;
import edu.mit.csail.sdg.translator.TranslateAlloyToKodkod;
import java.util.Scanner;
import kodkod.engine.satlab.SATFactory;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

public class RunAlloy {

  private RunAlloy() {
    throw new UnsupportedOperationException("Utility class");
  }

  /**
   * Calls Alloy evaluating command line arguments to limit search and choose solver to use.
   *
   * @param args command line arguments
   */
  public static void main(final String[] args) throws Err {

    final Options cmdOptions = new Options();
    cmdOptions.addOption(
        new Option("i", "instances", true, "Limit maximal number of instances to given number"));
    cmdOptions.addOption(new Option("o", "overflow", false, "Enable overflow"));
    cmdOptions.addOption(new Option("s", "solver", true, "SAT solver to use"));

    CommandLineParser parser = new DefaultParser();
    try {
      CommandLine cmd = parser.parse(cmdOptions, args);
      runAlloy(cmd);
    } catch (ParseException exp) {
      System.err.println(exp.getMessage());
    }
  }

  private static void runAlloy(final CommandLine cmd) throws Err {
    String cmdMaxInstances = cmd.getOptionValue("i");
    int maxInstances;
    if (cmdMaxInstances == null) {
      maxInstances = -1;
    } else {
      maxInstances = Integer.parseInt(cmdMaxInstances);
    }

    Scanner scanner = new Scanner(System.in);
    String content = "";
    while (scanner.hasNext()) {
      content = content + scanner.nextLine() + "\n";
    }

    A4Reporter reporter = new A4Reporter();
    Module module = CompUtil.parseEverything_fromString(reporter, content);

    A4Options options = new A4Options();
    if (cmd.hasOption("o")) {
      options.noOverflow = false;
    } else {
      options.noOverflow = true;
    }
    String cmdSatSolver = cmd.getOptionValue("s");
    try {
      options.solver = SATFactory.get(cmdSatSolver);
    } catch (IllegalArgumentException e) {
      e.printStackTrace();
      System.err.println(e);
      System.exit(2);
    }

    for (Command command : module.getAllCommands()) {
      int i = 0;
      A4Solution ans =
          TranslateAlloyToKodkod.execute_command(
              reporter, module.getAllReachableSigs(), command, options);
      if (options.solver.incremental()) {
        while (maxInstances != i++ && ans.satisfiable()) {
          if (ans.satisfiable()) {
            System.out.print(ans);
          }
          ans = ans.next();
        }
      } else if (ans.satisfiable()) {
        System.out.print(ans);
      }
    }
  }
}
