package alloy;

import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.ast.Module;
import edu.mit.csail.sdg.parser.CompUtil;
import edu.mit.csail.sdg.translator.A4Options;
import edu.mit.csail.sdg.translator.A4Solution;
import edu.mit.csail.sdg.translator.TranslateAlloyToKodkod;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import java.util.Scanner;

enum SATSolver {
    BERKMIN(A4Options.SatSolver.BerkMinPIPE, true), // not tested
    //CRYPTOMINISAT(A4Options.SatSolver.CryptoMiniSatJNI, ???), // not supported yet
    GLUCOSE(A4Options.SatSolver.GlucoseJNI, true),
    GLUCOSE41(A4Options.SatSolver.Glucose41JNI, true),
    LINGELING(A4Options.SatSolver.LingelingJNI, false),
    MINISAT(A4Options.SatSolver.MiniSatJNI, true),
    MINISAT_PROVER(A4Options.SatSolver.MiniSatProverJNI, true),
    PLINGELING(A4Options.SatSolver.PLingelingJNI, false),
    SAT4J(A4Options.SatSolver.SAT4J, true),
    SPEAR(A4Options.SatSolver.SpearPIPE, true); // not tested
    //TO_CNF(A4Options.SatSolver.CNF ??) // not suitable
    //TO_KODKOD(A4Options.SatSolver.KK ??) // not suitable

    private A4Options.SatSolver solver;
    private boolean incremental;
    public A4Options.SatSolver getSolver() {
        return this.solver;
    }
    public boolean isIncremental() {
        return this.incremental;
    }
    private SATSolver(A4Options.SatSolver solver, boolean incremental) {
        this.solver = solver;
        this.incremental = incremental;
    }
}

public class RunAlloy {

    public static void main(String[] args) throws Err {

        final Options cmdOptions = new Options();
        cmdOptions.addOption(new Option("i", "instances", true, "Limit maximal number of instances to given number"));
        cmdOptions.addOption(new Option("o", "overflow", false, "Enable overflow"));
        cmdOptions.addOption(new Option("s", "solver", true, "SAT solver to use"));

        CommandLineParser parser = new DefaultParser();
        try {
            CommandLine cmd = parser.parse(cmdOptions, args);
            runAlloy(cmd);
        }
        catch(ParseException exp) {
            System.err.println(exp.getMessage());
        }
    }
    private static void runAlloy(CommandLine cmd) throws Err {
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
        SATSolver solver = null;
        for (SATSolver x : SATSolver.values()) {
            if (cmdSatSolver.equals(x.name())) {
                solver = x;
            }
        }
        if (solver == null) {
            System.out.println("No solver or invalid solver chosen. Choose one of:");
            for (SATSolver x : SATSolver.values()) {
                System.out.println("  " + x.name());
            }
            System.exit(2);
        } else {
            options.solver = solver.getSolver();
        }

        for (Command command: module.getAllCommands()) {
            int i = 0;
            A4Solution ans = TranslateAlloyToKodkod.execute_command(reporter, module.getAllReachableSigs(), command, options);
            if (solver.isIncremental()) {
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
