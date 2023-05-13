with open('raw-potential-words.txt') as file:
    cleaned_lines = [x + '.\n' for x in filter(lambda x : len(x) == 5 and str(x).isalnum(), [x.replace('\n','') for x in file.readlines()])]
    cleaned_lines[-1:] = cleaned_lines[-1:][0].removesuffix('\n')
    with open('potential-words.txt', 'w') as target_file:
        target_file.writelines(cleaned_lines)