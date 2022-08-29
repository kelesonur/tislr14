from itertools import count
import pandas as pd

df = pd.read_csv('TISLR_Data.csv')

def CalculateAccessibility(data):
    ref = data["Referent_Name"].values.tolist()
    narrative = data["Narrative"].values.tolist()
    discourse = data["Discourse"].values.tolist()
    reference = data["Reference"].values.tolist()

    for i in range(len(ref)):
        lst = [index for index, j in zip(count(), ref) if j == ref[i]]
        score = 0
        # Scale for Previous Mention
        if narrative[i] == narrative[i-1] \
                and (ref[i] == ref[i-1] or discourse[i] == "Maintenance"):
            score = 3
        elif ref[i] != ref[i-1] and discourse[i] != "Introduction":
            score = 2
        elif discourse[i] == "Introduction" and \
                any(y < i for y in lst) and reference[i] < 4 \
                and (ref[i] != ref[i-1] or narrative[i] != narrative[i-1]) \
                and ref[i] != "White cat" and ref[i] != "Stairs" \
                and ref[i] != "Fish" and ref[i] != "Bird" \
                and ref[i] != "Carpet" and ref[i] != "Toy house" and ref[i] != "Toy girl" and narrative[i] != 1:
            score = 1
        else:
            score = 0
        # Scale for Topicality
        if ref[i-4:i].count(ref[i]) >= 3 and reference[i] > 2 and\
                ((narrative[i] == narrative[i-4])
                 or (narrative[i] == narrative[i-3]
                     and reference[i] == 4 and ref[i] == ref[i-3]
                     and ref[i] == ref[i-2])):
            score += 2
        elif ref[i-4:i].count(ref[i]) >= 1 and reference[i] > 1 and\
                ((narrative[i] == narrative[i-2]) or
                 (narrative[i] == narrative[i-1] and ref[i] == ref[i-1])):
            score += 1
        else:
            score += 0
        # # Scale for Competition
        if ref[i] != ref[i-1] and ref[i] != ref[i-2] \
                and reference[i] > 2 and ref[i-1] != ref[i-2]:
            score -= 2
        elif ref[i] != ref[i-1] and reference[i] > 1\
                and (ref[i-1] == ref[i-2] or ref[i] == ref[i-2] or reference[i] == 2):
            score -= 1
        else:
            score += 0
        print(score)

CalculateAccessibility(df)
