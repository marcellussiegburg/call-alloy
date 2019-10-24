package alloy;

import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.ast.Module;
import edu.mit.csail.sdg.translator.A4Options;
import edu.mit.csail.sdg.translator.A4Solution;
import edu.mit.csail.sdg.translator.TranslateAlloyToKodkod;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.parser.CompUtil;

import java.util.Scanner;

public class RunAlloy {
    public static void main(String[] args) throws Err {

        int maxRuns;
        if (args.length > 0) {
            maxRuns = Integer.parseInt(args[0]);
        } else {
            maxRuns = -1;
        }

        Scanner scanner = new Scanner(System.in);
        String content = "";
        while (scanner.hasNext()) {
            content = content + scanner.nextLine() + "\n";
        }

        A4Reporter reporter = new A4Reporter();
        Module module = CompUtil.parseEverything_fromString(reporter, content);

        A4Options options = new A4Options();
        options.noOverflow = true;
        options.solver = A4Options.SatSolver.SAT4J;

        for (Command command: module.getAllCommands()) {
            int i = 0;
            A4Solution ans = TranslateAlloyToKodkod.execute_command(reporter, module.getAllReachableSigs(), command, options);
            while (maxRuns != i && ans.satisfiable()) {
                if (ans.satisfiable()) {
                    System.out.print(ans);
                }
                ans = ans.next();
            }
        }
    }
}
