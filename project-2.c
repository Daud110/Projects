#include "project.h"


Character *add_character(Character *head, const char *name, int hit_points, const char *weapon_name, int weapon_damage) {
    if (!name || !weapon_name) {
        printf("Invalid command A %s\n",name ? name : "");   // using ternary because nothing else works
        return NULL;
    }

    if (find_character(head, name) != NULL) {
        printf("Fighter \"%s\" is already in the database.\n", name);
        return NULL;
    }

    Character *new_character = malloc(sizeof(Character));
    if (new_character == NULL) {
        printf("Failed to allocate memory for new character\n");
        return NULL;
    }

    new_character->name = malloc(strlen(name) + 1);
    if (new_character->name == NULL) {
        printf("Failed to allocate memory for new character's name\n");
        free(new_character);
        return NULL;
    }
    strcpy(new_character->name, name);

    new_character->hit_points = hit_points;
    new_character->experience = 0;

    new_character->weapon.name = malloc(strlen(weapon_name) + 1);
    if (new_character->weapon.name == NULL) {
        printf("Failed to allocate memory for new character's weapon name\n");
        free(new_character->name);
        free(new_character);
        return NULL;
    }
    strcpy(new_character->weapon.name, weapon_name);

    new_character->weapon.damage = weapon_damage;
    new_character->next = head;

    return new_character;
}


Character *find_character(Character *head, const char *name) {
    Character *current = head;
    while (current != NULL) {
        if (strcmp(current->name, name) == 0) {
            return current;
        }
        current = current->next;
    }
    return NULL;
}

int attack(Character *attacker, Character *target) {
    if (attacker == NULL || target == NULL) {
        printf("Invalid command H\n");
        return 0;
    }

    if (attacker == target) {
        printf("Invalid command H %s %s\n", attacker->name, target->name);
        return 0;
    }

    int damage = attacker->weapon.damage;
    target->hit_points -= damage;
    attacker->experience += damage;

    printf("%s attacked %s with %s by %d damage.\n", attacker->name, target->name, attacker->weapon.name, damage);
    printf("%s has %d hit points remaining.\n", target->name, target->hit_points);
    printf("%s gained %d experience points.\n", attacker->name, damage);

    return 1;
}


void print_game(Character *head, FILE *stream) {
    // Count the number of characters in the linked list
    int num_characters = 0;
    Character *current = head;
    while (current != NULL) {
        num_characters++;
        current = current->next;
    }

    // Create an array of characters and copy the linked list into it
    Character **character_array = malloc(num_characters * sizeof(Character *));
    current = head;
    for (int i = 0; i < num_characters; i++) {
        character_array[i] = current;
        current = current->next;
    }

    // Sort the array of characters by experience points (in descending order)
    Character *temp;
    for (int i = 0; i < num_characters - 1; i++) {
        for (int j = i + 1; j < num_characters; j++) {
            if (character_array[i]->experience < character_array[j]->experience) {
                // Swap the two elements
                temp = character_array[i];
                character_array[i] = character_array[j];
                character_array[j] = temp;
            }
        }
    }

    // Print out the characters in the sorted array
    for (int i = 0; i < num_characters; i++) {
        if (fprintf(stream, "%s %d %d %s %d\n", character_array[i]->name, character_array[i]->hit_points, character_array[i]->experience, character_array[i]->weapon.name, character_array[i]->weapon.damage) < 0) {
            printf("Failed to write character %s to file\n", character_array[i]->name);
            break;
        }
    }

    // Free the memory allocated for the array
    free(character_array);
}


int save_to_file(Character *head, const char *filename) {
    FILE *file = fopen(filename, "w");
    if (file == NULL) {
        printf("Invalid command W\n");
        return 0;
    }

    // Write each character's information to the file
    Character *current = head;
    while (current != NULL) {
        fprintf(file, "%s %d %d %s %d\n", current->name, current->hit_points, current->experience, current->weapon.name, current->weapon.damage);
        current = current->next;
    }

    fclose(file);

    return 1;
}


Character *load_from_file(const char *filename) {
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        printf("Cannot open file %s for reading.\n", filename);
        return NULL;
    }

    Character *head = NULL;
    char name[256];
    int hit_points, experience, weapon_damage;
    char weapon_name[256];

    while (fscanf(file, "%s %d %d %s %d", name, &hit_points, &experience, weapon_name, &weapon_damage) == 5) {
        head = add_character(head, name, hit_points, weapon_name, weapon_damage);
        Character *character = find_character(head, name);
        if (character != NULL) {
            character->experience = experience;
        }
    }

    fclose(file);
    if (head != NULL) {
    }
    return head;
}


void free_characters(Character *head) {
    Character *current = head;
    while (current != NULL) {
        Character *next = current->next;
        free(current->name);
        free(current->weapon.name);
        free(current);
        current = next;
    }
}


int main() {
    Character *head = NULL;
    char command_line[1001];

    while (1) {
        fgets(command_line, sizeof(command_line), stdin);
        if (strlen(command_line) > 0 && command_line[strlen(command_line) - 1] == '\n') {
            command_line[strlen(command_line) - 1] = '\0'; // Remove the newline character
        }

        if (strlen(command_line) == 0) {
            continue; // Skip empty commands
        }

        char *args = strchr(command_line, ' ');
        if (args != NULL) {
            *args = '\0'; // Split the command and arguments
            args++;
        }

        if (strcmp(command_line, "A") == 0) {
            char name[256], weapon_name[256];
            int hit_points, weapon_damage;
            if (sscanf(args, "%s %d %s %d", name, &hit_points, weapon_name, &weapon_damage) == 4) {
                if (hit_points < 1) {
                    printf("HP cannot be lower than 1\n");
                } else if (weapon_damage < 1) {
                    printf("Max damage cannot be lower than 1\n");
                } else if (strlen(weapon_name) == 0 || strspn(weapon_name, " ") == strlen(weapon_name)) {
                    printf("Weapon name cannot be empty or consist of only spaces.\n");
                } else {
                    Character *new_character = add_character(head, name, hit_points, weapon_name, weapon_damage);
                    if (new_character != NULL) {
                        head = new_character;
                        printf("SUCCESS\n");
                    }   
                }
            } else if (args == NULL) {
                printf("Invalid command A\n");
            } else {
                    printf("A should be followed by exactly 4 arguments.\n");
            }

        } else if (strcmp(command_line, "H") == 0) {
            char attacker_name[256], target_name[256];
            if (sscanf(args, "%s %s", attacker_name, target_name) == 2) {
                Character *attacker = find_character(head, attacker_name);
                Character *target = find_character(head, target_name);
                if (attacker == NULL) {
                    printf("Attacker \"%s\" is not in the database.\n", attacker_name);
                } else if (target == NULL) {
                    printf("Target \"%s\" is not in the database.\n", target_name);
                } else if (attacker == target) {
                    printf("Attacker \"%s\" cannot attack to itself.\n", attacker->name);
                } else {
                    attack(attacker, target);
                    printf("SUCCESS\n");
                }
            } else {
                printf("H should be followed by exactly 2 arguments.\n");
            }

        } else if (strcmp(command_line, "L") == 0) {
            print_game(head, stdout);
            printf("SUCCESS\n");
        } else if (strcmp(command_line, "W") == 0) {
            if (args != NULL) {
                if (save_to_file(head, args)) {
                    printf("SUCCESS\n");
                } else {
                    printf("Invalid command W %s\n", args);
                }
            } else {
                printf("Invalid command W\n");
            }
        } else if (strcmp(command_line, "O") == 0) {
            if (args != NULL) {
                FILE *file = fopen(args, "r");
                if (file == NULL) {
                    printf("Cannot open file %s for reading.\n", args);
                } else {
                    Character *new_head = load_from_file(args);
                    if (new_head != NULL) {
                        free_characters(head);
                        head = new_head;
                        printf("SUCCESS\n");
                    } else {
                        printf("Invalid command O %s\n", args);
                    }
                        fclose(file);
                }
            } else {
                printf("Invalid command O\n");
            }
        } else if (strcmp(command_line, "Q") == 0) {
	        free_characters(head);
	        printf("SUCCESS\n");
	        break;
	        } else {
	            printf("Invalid command %s\n", command_line);
	        }
	    }
        

    return 0;
}