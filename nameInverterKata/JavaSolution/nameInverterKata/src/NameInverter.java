import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class NameInverter {
    String invertName(String name) {
        if (name == null || name.length() <= 0)
            return "";

        return formatName(removeHonorifics(splitNames(name)));
    }

    ArrayList<String> splitNames(String name) {
        return new ArrayList<>(Arrays.asList(name.trim().split("\\s+")));
    }

    ArrayList<String> removeHonorifics(ArrayList<String> names) {
        if (names.size() > 1 && isHonorific(names.get(0)))
            names.remove(0);

        return names;
    }

    boolean isHonorific(String word) {
        return word.matches("Mr\\.|Mrs\\.|Dr\\.");
    }

    String formatName(ArrayList<String> names) {
        if (names.size() == 1)
            return names.get(0);

        return formatMultiElementName(names);
    }

    String formatMultiElementName(ArrayList<String> names) {
        String postNominal = getPostNominals(names);
        String firstName = names.get(0);
        String lastName = names.get(1);
        return String.format("%s, %s %s", lastName, firstName, postNominal).trim();
    }

    String getPostNominals(ArrayList<String> names) {
        String postNominal = "";
        if (names.size() > 2) {
            List<String> postNominals = names.subList(2, names.size());
            for (String pn : postNominals)
                postNominal += pn + " ";
        }
        return postNominal;
    }
}