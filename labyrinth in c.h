#ifndef _PROJECT__H_
#define _PROJECT__H_

#endif //! _PROJECT__H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Weapon {
    char *name;
    int damage;
} Weapon;

typedef struct Character {
    char *name;
    int hit_points;
    int experience;
    Weapon weapon;
    struct Character *next;
} Character;

Character *add_character(Character *head, const char *name, int hit_points, const char *weapon_name, int weapon_damage);
Character *find_character(Character *head, const char *name);
int attack(Character *attacker, Character *target);
void print_game(Character *head, FILE *stream);
int save_to_file(Character *head, const char *filename);
Character *load_from_file(const char *filename);
void free_characters(Character *head);
