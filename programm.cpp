#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <stdio.h>
#include <cstdlib>
#include <ctime>
#include <chrono>
#include <cmath>
#include <algorithm>
#include<string>

using namespace std;
    
double better_than = 0.6;//параметр для прохождения confidence

//сортировка строк
void swap_rows(double** a, int r1, int r2)
{
    double* b = a[r1];
    a[r1] = a[r2];
    a[r2] = b;
}

//сортировка строк двух массивов
void swap_rows_two_mas(double* a, int r1, int r2, double* a2)
{
    double b = a[r1];
    a[r1] = a[r2];
    a[r2] = b;

    double b2 = a2[r1];
    a2[r1] = a2[r2];
    a2[r2] = b2;
}

//Факториал 
long double fact(int N)
{
    if (N < 0) 
        return 0; 
    if (N == 0) 
        return 1; 
    else 
    
    return N * fact(N - 1);
}

//Евклидово расстояние двух точек 
double EuclideanDistance(double* x, double* y, int columnNumber)
{
    double diff = 0;
    double d = 0;

    for (int i = 0; i < columnNumber; i++)
    {
        diff = 0;
        diff = (x[i] - y[i])*(x[i] - y[i]);
        d = d + diff;
    }

    return sqrt(d);
}

//задание не целого случайного числа из промежутка
double xrand(double min, double max)
{
	return min + double(rand()) / double(RAND_MAX) * (max - min);
}

//случайная перемешивание массивов с данными и классами
void shuffle(double** arr, double* arr2, int N, int C)//arr,arr2 - массивы для перестановки, N - количество строк, С - количество столбцов
{
    // инициализация генератора случайных чисел
    srand(time(NULL));
 
    // реализация алгоритма перестановки
    for (int i = N - 1; i >= 1; i--)
    {
        int j = rand() % (i + 1);
 
        double tmp2 = arr2[j];
        arr2[j] = arr2[i];
        arr2[i] = tmp2;

        for (int g = 0; g < C; g++)
        {
            double tmp = arr[j][g];
            arr[j][g] = arr[i][g];
            arr[i][g] = tmp;
        }
    }
}

//УНИВЕРСАЛЬНЫЙ ТЕРМ
double term_universal(double x, int current_term)
{
    double t = 0;
    int term = 14;

    if (current_term > 0 && current_term < 3)
        term = 2;
    else if (current_term > 2 && current_term < 6)
    {
        term = 3;
        current_term = current_term - 2;
    }
    else if (current_term > 5 && current_term < 10)
    {
        term = 4;
        current_term = current_term - 5;
    }
    else if (current_term > 9 && current_term < 15)
    {
        term = 5;
        current_term = current_term - 9;
    }

    double v1 = (double)current_term / ((double)term - 1.0);

    if (current_term == term && x > 1)
        return 1;
    else if (current_term == 1 && x < 0)
        return 1;
    else if (x < ((double)current_term / ((double)term - 1.0)))
        t = x * (term - 1) - current_term + 2;
    else
        t = -x * (term - 1) + current_term;
    

    t = max(min(t, 1.0), 0.0);
    return t;
}

//создание правила
void create_rule(int num_term, int columnNumber, double** x, int* rule, int* random_objects, int ran)
{
    double* vero_fit = new double[num_term];
    double** m_allterm = new double*[num_term];
    for (int i = 0; i < num_term; i++)
    {
        m_allterm[i] = new double[columnNumber];
    }

    //обнуление
    for (int i = 0; i < num_term; i++)
    {
        vero_fit[i] = 0;
        for (int j = 0; j < columnNumber; j++)
        {
            m_allterm[i][j] = 0;
        }
    }

    for (int i = 0; i < ran; i++)
    {
        //проходим по всем его термам
        for (int l = 0; l < num_term; l++)
        {
            //для каждой переменной смотрим под какие нечеткие термы эта переменная подходит и с какой степенью
            for (int j = 0; j < columnNumber; j++)
            {
                //запоминаем все степени принадлежности переменной в массив
                m_allterm[l][j] += term_universal(x[random_objects[i]][j], l + 1);
            }
        }
    }

    for (int l = 0; l < columnNumber; l++)
    {
        //пропорциональная селекция
        double sum_fit = 0;

        for (int j = 0; j < num_term; j++)
        {
            sum_fit += m_allterm[j][l]/(double)ran;
        }

        for (int j = 0; j < num_term; j++)
        {
            if (sum_fit != 0)
            {
                vero_fit[j] = m_allterm[j][l]/(double)ran / sum_fit;
            }
            else
            {
                vero_fit[j] = 1.0 / (double)num_term;
            }
        }

        double sum_interval = 0;
        double prand = xrand(0, 1);
        for (int j = 0; j < num_term; j++)
        {
            if (sum_interval < prand && prand < (sum_interval + vero_fit[j]))
            {
                rule[l] = j + 1;
            }
            sum_interval += vero_fit[j];
        }
    }
    //с вероятностью > 0.5 заменяем на don't care 
    for (int j = 0; j < columnNumber; j++)
    {
        double change = xrand(0, 1);
        if (change > 0.5)
        {
            rule[j] = 0;
        }
    }

    for (int i = 0; i < num_term; i++)
    {
        delete m_allterm[i];
    }
    delete[] m_allterm;
    delete[] vero_fit;
}

//Подаем правило, выводим мю для такого-то класса
double check_conf_rule(double* x, int* rule_gen, int col)
{
    double min = 1;
    int flag = 0;
    for (int b = 0; b < col; b++)
    {
        double temp = 0;
        if (rule_gen[b] == 0)
        {
            temp = 1;
            flag++;
        }
        else
        {
            temp = term_universal(x[b], rule_gen[b]);
        }

        if (temp < min)
        {
            min = temp;
        }
    }
    if (flag == col)
    {
        min = 0;
    }
    
    return min;
}

//confidence measure of the fuzzy association rule
void confidence(int num_class, int lineNumber, int columnNumber, double* class_answers, double** data, int* rule, double* confid)
{
    //расчитываем конфиденс
    double* m = new double[num_class];

    //обнуление
    for (int j = 0; j < num_class; j++)
    {
        m[j] = 0;
    }

    double sum_m = 0;
    for (int j = 0; j < lineNumber; j++)
    {
        double k = 0;
        k = check_conf_rule(data[j], rule, columnNumber);

        m[(int)class_answers[j]] += k;

        sum_m += k;
    }

    for (int j = 0; j < num_class; j++)
    {
        confid[j] = m[j] / sum_m;
        //cout << confid[j] << " " ;
    }
    //cout << endl;
    delete[] m;
}

//База правил подаем координаты, возвращает номер класс
void Rules(double* x, int** rules_gen, int* class_rule, double* confid_rules, int col, int num_rules, int** best_rule_for_object, double** reply, int y, int j)
{
    double max = 0;
    int number_rule_best = -1;
    int checki = 0;
    for (int p = 0; p < num_rules; p++)
    {
        if (confid_rules[p] > better_than)
        {
            double min = 1;
            for (int b = 0; b < col; b++)
            {
                double temp = 0;
                if (rules_gen[p][b] == 0)
                {
                    temp = 1;
                }
                else
                {
                    temp = term_universal(x[b], rules_gen[p][b]);
                }

                if (temp < min)
                {
                    min = temp;
                }
            }
            //Процедура argmax
            if (min > max)
            {
                max = min;
                number_rule_best = p;
            }
        }
    }
    /*if (class_rule[number_rule_best] != 0 && class_rule[number_rule_best] != 1)
    {
        cout << class_rule[number_rule_best];
    }*/
    best_rule_for_object[y][j] = number_rule_best;
    checki = class_rule[number_rule_best];
    if (number_rule_best == -1)
    {
        checki = -1;
    }
    reply[y][j] = checki;
}

//Количество верно классифицированных объектов одним правилом
void check_fitness_michegan(int* correct_classification_for_object_train, int* best_rule_for_object_train, int y, int num_rules, int linenumber, int** fitness_michegan)
{
    for (int i = 0; i < linenumber; i++)
    {
        /*if (correct_classification_for_object_train[i] == 1)
        {
            fitness_michegan[y][best_rule_for_object_train[i]]++;
        }*/
        for (int j = 0; j < num_rules; j++)
        {
            if ((best_rule_for_object_train[i] == j) && (correct_classification_for_object_train[i] == 1))
            {
                fitness_michegan[y][j] = fitness_michegan[y][j] + 1;
            }
        }
    }
}

//подсчет количества активных правил в популяции
int active_rule_flag(double* active_rules, int number_rules, double better_than)
{
    int flag = 0;
    for (int l = 0; l < number_rules; l++)
    {
        if (active_rules[l] > better_than)
        {
            flag++;
        }
    }
    return flag;
}

//подсчет кол-ва confid > 0,5
int confid_rule_flag(double* confid_rules, int number_rules, double better_than)
{
    int flag = 0;
    for (int l = 0; l < number_rules; l++)
    {
        if (confid_rules[l] > better_than)
        {
            flag++;
        }
    }
    return flag;
}

//don't care среднее в популяции по правиле
double dont_care_flag(int number_rules, int columnNumber, int** x, double* confid_rules, double better_than)
{
    int flag = 0;
    double average = 0;
    for (int i = 0; i < number_rules; i++)
    {
        if (confid_rules[i] > better_than)
        {
            flag = 0;
            for (int l = 0; l < columnNumber; l++)
            {
                if (x[i][l] != 0)
                {
                    flag++;
                }
            }
            average = average + (double)flag;
        }
    }
    return average;
}

//матрица ошибок для n классов
void error_matrix(int train_length, double* reply_train, double* train_class_answers, double* class_value, int num_class, int* class_values_count_train, int** mfalse, int didntgetclass)
{
    //подсчет количества каждого класса в массиве 
    int* class_values_count_reply = new int[num_class]; 

    //обнуление
    for (int i = 0; i < num_class; i++)
    {
        class_values_count_reply[i] = 0;
    }

    //подсчет количества повторяющихся значений для каждого класса в массиве
    for(int class_id = 0; class_id < num_class; class_id++)
    {
        int n = 0;
        for(int j = 0; j < train_length; j++)
        {		
            if(reply_train[j]==class_value[class_id]) 
            {
                n++;
            }	
        }
        class_values_count_reply[class_id] = n;	      
    } 
        
    for (int i = 0; i < train_length; i++)
    {
        if (reply_train[i] == -1)
        {
            mfalse[(int)train_class_answers[i]][num_class] = mfalse[(int)train_class_answers[i]][num_class] + 1; 
            didntgetclass = didntgetclass + 1;
        }
        else
        {
            if (reply_train[i] == train_class_answers[i])
            {
                mfalse[(int)reply_train[i]][(int)reply_train[i]] = mfalse[(int)reply_train[i]][(int)reply_train[i]] + 1;
            }
            if (reply_train[i] != train_class_answers[i])
            {
                mfalse[(int)train_class_answers[i]][(int)reply_train[i]] = mfalse[(int)train_class_answers[i]][(int)reply_train[i]] + 1;
            }
        }
    }
    
    delete[] class_values_count_reply;
}

//ранговая селекция
double rank_selection (double* fitness, int pop_size, double* rank)
{
    double* fitness_watch = new double[pop_size];
    
    //обнуление
    for (int j = 0; j < pop_size; j++)
    {
        fitness_watch[j] = 0;
    }

    for (int j = 0; j < pop_size; j++)
    {
        fitness_watch[j] = j;
    }

    for(int i = 0; i < pop_size-1; i++)
    {
        // Поиск наименьшего в первом столбце
        double m = fitness[i];
        int idx = i;
        for(int j = i; j < pop_size; j++)
        {
            if (fitness[j] < m) 
            {
                m = fitness[j];
                idx = j;
                // Обмен
                swap_rows_two_mas(fitness, i, idx, fitness_watch);
            }
        }
    }

    for (int i = 0; i < pop_size; i++)
    {
        rank[i] = i + 1;//ранг с 1 начинается
    }

    int repeat = 0;
    for (int counter_i = 0; counter_i < pop_size - 1; counter_i+= repeat)
    {
        //повторяющиеся значения среднее
        repeat = count(fitness, fitness+pop_size, fitness[counter_i]);
              
        if (repeat > 1)
        {
            int sum = 0;
            for (int j = counter_i; j < counter_i + repeat; j++)
            {
                sum = sum + rank[j];
            }

            for (int j = counter_i; j < counter_i + repeat; j++)
            {
                rank[j] = sum/repeat;
            }
        }
    } 

    for(int i = 0; i < pop_size-1; i++)
    {
        double minin = fitness_watch[i];
        int idx = i;
        for(int j = i; j < pop_size; j++)
        {
            if (fitness_watch[j] < minin) 
            {
                minin = fitness_watch[j];
                idx = j;
                // Обмен
                swap_rows_two_mas(fitness_watch, i, idx, rank);
            }
        }
    }

    delete[] fitness_watch;

    return 0;
}

//Fitness Michegan Part
void checkz_fitness_michegan(int* correct_classification_for_object_train, int* best_rule_for_object_train, int y, int num_rules, int linenumber, int** fitness_michegan)
{
    for (int i = 0; i < linenumber; i++)
    {
        for (int j = 0; j < num_rules; j++)
        {
            if ((best_rule_for_object_train[i] == j) && (correct_classification_for_object_train[i] == 1))
            {
                fitness_michegan[y][j] = fitness_michegan[y][j] + 1;
            }
        }
    }
}

int main (int argc, char* argv[]) {
    srand(time(0));//в самом начале один раз для рандома
    //chrono::steady_clock sc;   // создание объекта `steady_clock` class
    //auto start = sc.now();     // старт таймера
//--------------------------------------работа с файлом-------------------------------------------------------------------
    /*string file_1 = "10phoneme.txt";
    string file_2 = "9ring.txt";
    string file_3 = "16banknote.txt";
    string file_4 = "19segment_n.txt";
    string file_5 = "21texture.txt";
    string file_6 = "7page-blocks_n.txt";
    string file_7 = "5satimage_n.txt";*/
    
    
    //получение кол-ва строк и столбов
    //запись данных в массив
    string line, csvItem;
    string read_file;
    read_file = "1Australian.txt";
    //cout << "Please entry file name and his extension (for example, iris.txt) - ";
    //cin >> read_file;
    /*if (argc == 1)
    {
        read_file = file_1;
    }
    if (argc == 2)
    {
        read_file = file_2;
    }
    if (argc == 3)
    {
        read_file = file_3;
    }
    if (argc == 4)
    {
        read_file = file_4;
    }
    if (argc == 5)
    {
        read_file = file_5;
    }
    if (argc == 6)
    {
        read_file = file_6;
    }
    if (argc == 7)
    {
        read_file = file_7;
    }
    else 
    {
        cout << "Not arguments" << endl;
    }
    cout << read_file ;*/
    ifstream myfile (read_file.c_str());//считываение файла
    int lineNumber = 0;//кол-во строк
    int columnNumberFile = 0;//количество столбцов во всем файле(параметры + класс)

    if (myfile.is_open()) 
    {
        while (getline(myfile,line)) 
        {
            lineNumber++;
            istringstream myline(line);
            int get_column = 0;
            while(getline(myline, csvItem, ' ')) 
            {
                get_column++;
            }
            columnNumberFile = get_column;
        }
        myfile.close();
    }
    //инициализация массива с данными
    double** file_mas = new double*[lineNumber];
    for (int i = 0; i < lineNumber; i++)
    {
        file_mas[i] = new double[columnNumberFile];
    }
    
    ifstream myfile2 (read_file.c_str());//открываем второй раз для получения данных

    if (myfile2.is_open()) 
    {
        for (int i = 0; i < lineNumber; i++)
	    {
            for (int j = 0; j < columnNumberFile; j++)
	        {
                myfile2 >> file_mas[i][j];
            }
	    }
        /*
        //вывод в терминал массива
        for (int i = 0; i < lineNumber; i++)
	    {
            for (int j = 0; j < columnNumberFile; j++)
	        {
                cout << file_mas[i][j] << " ";
            }
            cout << endl;
	    }*/
        myfile2.close();
    }
    //запись в массив классов и объектов
    int columnNumber = columnNumberFile - 1;//количество столбцов для параметров
    //массив, в котором лежат классы для каждого объекта 
    double* class_answers = new double[lineNumber];
    for (int i = 0; i < lineNumber; i++)
    {
        class_answers[i] = file_mas[i][columnNumber];
    }
    //массива с объектами без классов
    double** data_file = new double*[lineNumber];
    for (int i = 0; i < lineNumber; i++)
    {
        data_file[i] = new double[columnNumber];
    }

    for (int i = 0; i < lineNumber; i++)
    {
        for (int j = 0; j < columnNumber; j++)
        {
            data_file[i][j] = file_mas[i][j];
        }
    }/*
    //вывод в терминал массивов
    for (int i = 0; i < lineNumber; i++)
    {
        for (int j = 0; j < columnNumber; j++)
        {
            cout << data_file[i][j] << " ";
        }
        cout << endl;
    }
    
    for (int i = 0; i < lineNumber; i++)
    {
        cout << class_answers[i] << " ";
    }*/
    //операции с массивом классов (подсчет кол-ва классов и кол-ва объектов конкретного класса)
    int num_class = 0;//количество значений классов
    //подсчет количества уникальных значений в массиве классов 
    for (int i = 0; i < lineNumber; ++i)
    {
        int j;
        for (j = i + 1; j < lineNumber && class_answers[j] != class_answers[i]; ++j);
        num_class += j == lineNumber;      
    }
    //std::cout << "Number of classes " << num_class << std::endl;

    double* class_value = new double[num_class];
    int* class_values_count = new int[num_class];

    //подсчет количества повторяющихся значений для каждого класса
    int class_count = 0;
    for(int i = 0; i < lineNumber; i++ ){
		int n = 0;
		for(int j = 0; j < lineNumber; j++){
			if(class_answers[i]==class_answers[j]) 
            {
				if(i > j)	break;
				n++;
			}					
		}
		//if(n) std::cout << class_answers[i] << " " << n << " times\n";
        if(n)
        {
            class_value[class_count] = class_answers[i];
            class_values_count[class_count] = n;
            class_count++;
        }	
	}
    
    //вывод повторяющихся значений класса
    /*for (int i = 0; i < num_class; i++)
    {
        cout << class_value[i] << " ";
        cout << class_values_count[i] << " ";
        cout << "\n";
    }*/
//-------------------------------------------ключевые параметры---------------------------------------------------
    //кол-во индивидов
    int pop_size = 100;//потом спросить у пользователя
    //кол-во поколений
    int gen = 1000;//потом спросить у пользователя
    int max_gen = gen;
    //количество правил
    int number_rules = 50;
    //турнирная селекция
    int T = 2;
    int w1 = 10000;
    int w2 = 1;
    int w3 = 1;
    int npop = 4;
    //количество рандомных объектов, по которым провести турнирную селекцию для формирования правила
    int num_random = 15;
    //количество объектов, на основании которых создается правило
    int num_obj_create_rule = 10;
    int kfold = 10;
    int cross_num = lineNumber / kfold;
    int last_data = lineNumber % kfold;
    int cross_num_const = cross_num;
    int which_initial = 2;
    //0 - формирование правила с одного случайного объекта
    //1 - с n случайных объектов
    //2 - с n случайных объектов с минимальным евклидовым расстоянием
    int which_selection = 1;
    //0 - ранговая селекция 
    //1 - турнирная селекция с N размером
    int which_crossover = 1;
    //0 - равномерное скрещивание
    //1 - скрещивание с кол-вом верно классиф. объектов
    int which_mutation = 1;
    //0 - слабая
    //1 - средняя
    //2 - сильная

    //закомментировать
    string whatfileoutput;
    //whatfileoutput = to_string(which_initial) + to_string(which_selection) + to_string(which_crossover) + to_string(which_mutation) + ".txt";
    //в с++ system (exe + excel с настройкой)
    //написать код для обработки питон в jupytere
    //многокритериальная оптимизация
    double** count_average_kfold = new double*[10];
    for (int i = 0; i < 10; i++)
    {
        count_average_kfold[i] = new double[kfold];
    }

    int piece = 10;
    int didntgetclass = 0;
    int didntgetclasstest = 0;
//-----------------------------------------------нормировка------------------------------------------------------
    double** data = new double*[lineNumber];
    for (int i = 0; i < lineNumber; i++)
    {
        data[i] = new double[columnNumber];
    }

    double* max_el = new double[columnNumber];
    double* min_el = new double[columnNumber];

    for (int i = 0; i < columnNumber; i++)//поиск для каждого параметра max элемент
    {
        max_el[i] = data_file[0][i];
        for (int j = 1; j < lineNumber; j++)
        {
            if (data_file[j][i] > max_el[i])
            {
                max_el[i] = data_file[j][i];
            }
        }
    }

    for (int i = 0; i < columnNumber; i++)//поиск для каждого параметра min элемент
    {
        min_el[i] = data_file[0][i];
        for (int j = 1; j < lineNumber; j++)
        {
            if (data_file[j][i] < min_el[i])
            {
                min_el[i] = data_file[j][i];
            }
        }
    }

    //нормировка
    for (int i = 0; i < columnNumber; i++)
    {
        for (int j = 0; j < lineNumber; j++)
        {
            if (max_el[i] == min_el[i])
            {
                data[j][i] = 0;
            }
            else
            {
                data[j][i] = (data_file[j][i] - min_el[i]) / (max_el[i] - min_el[i]);//новая матрица со значениями от 0 до 1
            }
        }
    }

    /*//вывод в терминал массива нормировки
    for (int i = 0; i < lineNumber; i++)
    {
        for (int j = 0; j < columnNumber; j++)
        {
            cout << data[i][j] << " ";
        }
        cout << endl;
    }*/
//---------------------------------------обучающая и тестовая выборки---------------------------------------------    
    //случайное перемешивание массивов с данными и с ответами соответственно
    shuffle(data, class_answers, lineNumber, columnNumber);

    //вывод в терминал перемешанных массивов
    /*for (int i = 0; i < lineNumber; i++)
    {
        for (int j = 0; j < columnNumber; j++)
        {
            cout << data[i][j] << " ";
        }
        cout << endl;
    }
    
    for (int i = 0; i < lineNumber; i++)
    {
        cout << class_answers[i] << " ";
    }
    //cout << cross_num;*/
    
    //перебор для вывода ВЫВЕСТИ В СВЕТ
//  for (int initialize = 1; initialize < 3; initialize++)
    {
        which_initial = 0;//базовая инициализация
        //for (int selection_int = 0; selection_int < 2; selection_int++)
        {
            which_selection = 1;//турнирная
            //for (int crossover_int = 0; crossover_int < 2; crossover_int++)
            {
                which_crossover = 0;//базования
                //for (int mutation_int = 0; mutation_int < 3; mutation_int++)
                {
                    which_mutation = 1;//средняя

                    string whatfileoutput;
                    whatfileoutput = to_string(which_initial) + to_string(which_selection) + to_string(which_crossover) + to_string(which_mutation) + ".txt";

    cout << whatfileoutput << endl;
    cross_num = cross_num_const;
    int train_length = lineNumber-(cross_num_const+last_data);
    int test_length = cross_num_const+last_data; 
    double** test_data = new double*[cross_num_const+last_data];
    for (int i = 0; i < cross_num_const+last_data; i++)
    {
        test_data[i] = new double[columnNumber];
    }
    double** train_data = new double*[lineNumber-(cross_num_const+last_data)];
    for (int i = 0; i < lineNumber-(cross_num_const+last_data); i++)
    {
        train_data[i] = new double[columnNumber];
    }
    double* test_class_answers = new double[test_length];
    double* train_class_answers = new double[train_length];

    double accuracy = 0;
    double best_accuracy = 0;
    double precision = 0;
    double best_precision = 0;
    //best_presi и все остальное best по train
    double recall = 0;
    double best_recall = 0;
    double Fscore = 0;
    double best_Fscore = 0;
    int numerator = 0;
    int denominator = 0; 
    int b = 1;

    double accuracy_test = 0;
    double precision_test = 0;
    double recall_test = 0;
    double Fscore_test = 0;

    double dont_care_file = 0;
    double num_rule_file = 0;

    double average_accuracy_kfold = 0;//точность на обучающей
    double average_accuracy_test_kfold = 0;//точность на тестовой 
    double average_num_rule_file_kfold = 0;//количество активных правил
    double average_dont_care_file_kfold = 0;//длина правила

    double average_precision_kfold = 0;
    double average_recall_kfold = 0; 
    double average_Fscore_kfold = 0;

    double average_precision_test_kfold = 0;
    double average_recall_test_kfold = 0; 
    double average_Fscore_test_kfold = 0;
    
    //обнуление
    for (int i = 0; i < 10; i++)
    {
        for (int j = 0; j < 10; j++)
        {
            count_average_kfold[i][j] = 0;
        }
    }

    for (int i = 0; i < kfold; i++)
    {
        cout << "Piece " << i << endl;

        //0 - pop
        //1 - pop2
        //2 - pop3
        int num_term = 14;    
        double*** confid_rules = new double** [npop];
        double*** weight_rules = new double** [npop];
        int*** active_rules = new int** [npop];
        int*** class_rules = new int** [npop];
        int*** rules_update = new int** [npop];

        for (int ip = 0; ip < npop; ip++)
        {
            confid_rules[ip] = new double*[pop_size];
            weight_rules[ip] = new double*[pop_size];
            active_rules[ip] = new int*[pop_size];
            class_rules[ip] = new int*[pop_size];
            rules_update[ip] = new int*[pop_size];
        
            for (int j = 0; j < pop_size; j++)
            {
                confid_rules[ip][j] = new double[number_rules];
                weight_rules[ip][j] = new double[number_rules];
                active_rules[ip][j] = new int[number_rules];
                class_rules[ip][j] = new int[number_rules];
                rules_update[ip][j] = new int[number_rules];
            }
        }
        for (int ip = 0; ip < npop; ip++)
        {
            for (int j = 0; j < pop_size; j++)
            {
                for (int l = 0; l < number_rules; l++)
                {
                    active_rules[ip][j][l] = 0;
                    class_rules[ip][j][l] = 0;
                    weight_rules[ip][j][l] = 0;
                    rules_update[ip][j][l] = 0;
                    confid_rules[ip][j][l] = 0;
                }
            }
        }

        int*** pop = new int** [pop_size];
        int*** pop2 = new int** [pop_size];
        int*** pop3 = new int** [pop_size];
        int*** out = new int** [pop_size];
        int** best_rule_for_object_train = new int*[pop_size];
        int** best_rule_for_object_test = new int*[pop_size];
        int** correct_classification_for_object_train = new int*[pop_size];
        int** fitness_michegan = new int*[pop_size];
        int** fitness_michegan2 = new int*[pop_size];
        int** correct_classification_num = new int*[pop_size];
        double* fitness = new double[pop_size];
        double* fitness_small = new double[pop_size];
        double* f_score_fit = new double[pop_size];
        double* accuracy_fit = new double[pop_size];
        double* num_rules_fit = new double[pop_size];
        double* fitness_rang = new double[pop_size];
        double* fitness_rang_prop = new double[pop_size];

        for (int ip = 0; ip < pop_size; ip++)
        {
            pop[ip] = new int*[number_rules];
            pop2[ip] = new int*[number_rules];
            pop3[ip] = new int*[number_rules];
            out[ip] = new int*[number_rules];
            best_rule_for_object_train[ip] = new int[lineNumber-(cross_num_const+last_data)];
            best_rule_for_object_test[ip] = new int[cross_num_const+last_data];
            fitness_michegan[ip] = new int[number_rules];
            fitness_michegan2[ip] = new int[number_rules];
            correct_classification_num[ip] = new int[number_rules];
            //правильно классифицированный объект - 1
            //неправильно классифицированный объект - 0
            correct_classification_for_object_train[ip] = new int[lineNumber-(cross_num_const+last_data)];

            for (int j = 0; j < number_rules; j++)
            {
                pop[ip][j] = new int[columnNumber];
                pop2[ip][j] = new int[columnNumber];
                pop3[ip][j] = new int[columnNumber];
                out[ip][j] = new int[columnNumber];
            }
        }
        for (int ip = 0; ip < pop_size; ip++)
        {
            for (int j = 0; j < number_rules; j++)
            {
                for (int l = 0; l < columnNumber; l++)
                {
                    pop[ip][j][l] = 0;
                    pop2[ip][j][l] = 0;
                    pop3[ip][j][l] = 0;
                    out[ip][j][l] = 0;
                }
            }
        }
        for (int ip = 0; ip < pop_size; ip++)
        {
            for (int j = 0; j < lineNumber-(cross_num_const+last_data); j++)
            {
                best_rule_for_object_train[ip][j] = 0;
            }
            for (int j = 0; j < number_rules; j++)
            {
                fitness_michegan[ip][j] = 0;
                correct_classification_num[ip][j] = 0;
            }
        }

        if (i == 0)
        {
            //тестовые данные
            for (int j = 0; j < cross_num+last_data; j++)
            {
                for (int g = 0; g < columnNumber; g++)
                {
                    test_data[j][g] = data[j][g];
                    test_class_answers[j] = class_answers[j];
                }
            } 
            //обучающие даные
            int counter = 0;
            for (int j = cross_num+last_data; j < lineNumber; j++)
            {
                for (int g = 0; g < columnNumber; g++)
                {
                    train_data[counter][g] = data[j][g];
                    train_class_answers[counter] = class_answers[j];
                }
                counter++;
            } 
        }
        else if (i != 0 && i != kfold-1)
        {
            //тестовые данные
            int counter = 0;
            for (int j = cross_num; j < cross_num+cross_num_const+last_data; j++)
            {
                for (int g = 0; g < columnNumber; g++)
                {
                    test_data[counter][g] = data[j][g];
                    test_class_answers[counter] = class_answers[j];
                }
                counter++;
            } 
            //обучающие даные
            for (int j = 0; j < cross_num; j++)
            {
                for (int g = 0; g < columnNumber; g++)
                {
                    train_data[j][g] = data[j][g];
                    train_class_answers[j] = class_answers[j];
                }
            } 
            counter = 0;
            for (int j = cross_num+cross_num_const+last_data; j < lineNumber; j++)
            {
                for (int g = 0; g < columnNumber; g++)
                {
                    train_data[counter][g] = data[j][g];
                    train_class_answers[counter] = class_answers[j];
                }
                counter++;
            } 
            cross_num = cross_num + cross_num_const;
        }
        else if (i == kfold-1)
        {
            //тестовые данные
            int counter = 0;
            for (int j = lineNumber-(cross_num_const+last_data); j < lineNumber; j++)
            {
                for (int g = 0; g < columnNumber; g++)
                {
                    test_data[counter][g] = data[j][g];
                    test_class_answers[counter] = class_answers[j];
                }
                counter++;
            } 
            //обучающие даные
            for (int j = 0; j < lineNumber-(cross_num_const+last_data); j++)
            {
                for (int g = 0; g < columnNumber; g++)
                {
                    train_data[j][g] = data[j][g];
                    train_class_answers[j] = class_answers[j];
                }
            } 
        }
        int check_gen = 0;

        //подсчет количества каждого класса в train выборке!!!
        int* class_values_count_train = new int[num_class];
        int* class_values_count_test = new int[num_class];

        //rule вынесен
        int* in_rule = new int[columnNumber];
        //inconf вынесен
        double* in_confid = new double[num_class];

        //обнуление
        for (int j = 0; j < num_class; j++)
        {
            class_values_count_train[j] = 0;
            class_values_count_test[j] = 0;
        }

        //задача подбора информативных признаков 

        //подсчет количества повторяющихся значений для каждого класса в train set
        for(int class_id = 0; class_id < num_class; class_id++)
        {
            int n = 0;
            for(int j = 0; j < train_length; j++)
            {
                if(train_class_answers[j]==class_value[class_id]) 
                {
                    n++;
                }					
            }
            class_values_count_train[class_id] = n;	
        } 
        //подсчет количества повторяющихся значений для каждого класса в test set
        for(int class_id = 0; class_id < num_class; class_id++)
        {
            int n = 0;
            for(int j = 0; j < test_length; j++)
            {
                if(test_class_answers[j]==class_value[class_id]) 
                {
                    n++;
                }					
            }
            class_values_count_test[class_id] = n;	
        } 

        //вывод повторяющихся значений класса для train set
        /*for (int m = 0; m < num_class; m++)
        {
            cout << class_value[m] << " ";
            cout << class_values_count_train[m] << " ";
            cout << "\n";
        }*/

        /*for (int i = 0; i < lineNumber-(cross_num+last_data); i++)
        {
            for (int j = 0; j < columnNumber; j++)
            {
                cout << train_data[i][j] << " ";//120
            }
            cout << endl;
        }*/
                
        /*for (int i = 0; i < cross_num+last_data; i++)
        {
            for (int j = 0; j < columnNumber; j++)
            {
                cout << test_data[i][j] << " ";//30
            }
            cout << endl;
        }
        break;*/

        //для вывода матрицы ошибок классификации
        int** mas_error_classification = new int* [num_class+1]; 
        for (int j = 0; j < num_class+1; j++)
        {
            mas_error_classification[j] = new int[num_class+1];
        }
//-------------------------------------------начало га-лгоритма----------------------------------------------------
//сбор данных для каждого индивида после инициалзации, после скрещивания и мутации, после мичиганской части
//все параметры индивида
//50 индивидов -> 50 правил и по ним графики лучшее правило, конфиденс, трейн, тест
        //инициализация
        int q_number = 0;
        if (which_initial == 0)
        {
            for (int ipop = 0; ipop < pop_size; ipop++)
            {
                //cout << i << " ";
                //cout << endl;
                if (columnNumber <= number_rules/2)
                {
                    q_number = columnNumber;
                }
                else
                {
                    q_number = number_rules/2;
                }
                
                int flag = 0;
                for (int q = 0; q < q_number; q++)
                {
                    for (int j = 0; j < columnNumber; j++)
                    {
                        in_rule[j] = 0;
                    }
                    //случайный объект из выборки
                    int random_class = -1;
                    random_class = rand() % num_class;
                    int random_obj = rand() % train_length;
                    while (train_class_answers[random_obj] != random_class)
                    {
                        random_obj = rand() % train_length;
                    }
                    int* obj_for_rule = new int[1];
                    num_obj_create_rule = 1;
                    obj_for_rule[0] = random_obj;
                    create_rule(num_term, columnNumber, train_data, in_rule, obj_for_rule, num_obj_create_rule);

                    /*for (int j = 0; j < columnNumber; j++)
                    {
                        cout << rule[j] << " ";
                    }*/

                    for (int j = 0; j < num_class; j++)
                    {
                        in_confid[j] = 0;
                    }
                    
                    confidence(num_class, train_length, columnNumber, class_answers, data, in_rule, in_confid);
                    /*
                    for (int j = 0; j < num_class; j++)
                    {
                        cout << in_confid[j] << " ";
                    }*/

                    /*cout << endl << " Правило " << endl;
                    for (int j = 0; j < columnNumber; j++)
                    {
                        cout << in_rule[j] << " ";
                    }*/

                    double max_confid = 0;
                    int c_confid = 0;
                    for (int j = 0; j < num_class; j++)
                    {
                        if (in_confid[j] > max_confid)
                        {
                            max_confid = in_confid[j];
                            c_confid = j;
                        }
                    }

                    if (max_confid > better_than)
                    {
                        flag++;
                        for (int y = 0; y < columnNumber; y++)
                        {
                            pop[ipop][q][y] = in_rule[y];
                        }
                        class_rules[0][ipop][q] = c_confid;
                        confid_rules[0][ipop][q] = max_confid;
                        weight_rules[0][ipop][q] = 2*max_confid - 1;
                        active_rules[0][ipop][q] = 1;
                    }
                    else
                    {
                        for (int y = 0; y < columnNumber; y++)
                        {
                            pop[ipop][q][y] = 0;
                        }
                        active_rules[0][ipop][q] = 0;
                        class_rules[0][ipop][q] = 0;
                        confid_rules[0][ipop][q] = 0;
                        weight_rules[0][ipop][q] = 0;
                    }

                    delete[] obj_for_rule;
                }
                if (flag < num_class)
                {
                    for (int q = 0; q < number_rules; q++)
                    {
                        if (active_rules[0][ipop][q] == 0)
                        {
                            //заменяем его на то, которое подходит
                            for (int j = 0; j < columnNumber; j++)
                            {
                                in_rule[j] = 0;
                            }
                            //случайный объект из выборки
                            int random_obj = rand() % train_length;
                            int* obj_for_rule = new int[1];
                            num_obj_create_rule = 1;
                            obj_for_rule[0] = random_obj;
                            create_rule(num_term, columnNumber, train_data, in_rule, obj_for_rule, num_obj_create_rule);

                            for (int j = 0; j < num_class; j++)
                            {
                                in_confid[j] = 0;
                            }

                            confidence(num_class, train_length, columnNumber, class_answers, data, in_rule, in_confid);

                            /*for (int j = 0; j < num_class; j++)
                            {
                                cout << in_confid[j] << " ";
                            }*/

                            double max_confid = 0;
                            int c_confid = 0;
                            for (int j = 0; j < num_class; j++)
                            {
                                if (in_confid[j] > max_confid)
                                {
                                    max_confid = in_confid[j];
                                    c_confid = j;
                                }
                            }

                            if (max_confid > better_than)
                            {
                                flag++;
                                for (int y = 0; y < columnNumber; y++)
                                {
                                    pop[ipop][q][y] = in_rule[y];
                                }
                                class_rules[0][ipop][q] = c_confid;
                                confid_rules[0][ipop][q] = max_confid;
                                weight_rules[0][ipop][q] = 2*max_confid - 1;
                                active_rules[0][ipop][q] = 1;
                            }

                            else
                            {
                                for (int y = 0; y < columnNumber; y++)
                                {
                                    pop[ipop][q][y] = 0;
                                }
                                active_rules[0][ipop][q] = 0;
                                class_rules[0][ipop][q] = 0;
                                confid_rules[0][ipop][q] = 0;
                                weight_rules[0][ipop][q] = 0;
                            }
                            delete[] obj_for_rule;
                        }
                        if (flag > num_class)
                        {
                            break;
                        }
                    }
                }
                /*
                for (int j = 0; j < number_rules; j++)
                {
                    for (int y = 0; y < columnNumber; y++)
                    {
                        cout << pop[i][j][y] << " ";
                    }
                    cout << endl;
                }
                
                for (int y = 0; y < number_rules; y++)
                {
                    cout << class_rules[0][i][y] << " ";
                    if (active_rules[0][i][y] == 0)
                    {
                        cout << "<-wrong ";
                    }
                }*/
                /*for (int y = 0; y < number_rules; y++)
                {
                    cout << "active " << active_rules[0][i][y] << endl;
                    cout << "confidence " << confid_rules[0][i][y] << endl;
                    cout << "weight " << weight_rules[0][i][y] << endl;
                }
                cout << endl;*/
            }
        }
        else if (which_initial == 1)
        {
            for (int ipop = 0; ipop < pop_size; ipop++)
            {
                //cout << i << " ";
                //cout << endl;
                if (columnNumber <= number_rules/2)
                {
                    q_number = columnNumber;
                }
                else
                {
                    q_number = number_rules/2;
                }
                
                int flag = 0;
                for (int q = 0; q < q_number; q++)
                {
                    for (int j = 0; j < columnNumber; j++)
                    {
                        in_rule[j] = 0;
                    }
                    //случайный объект из выборки
                    int* obj_for_rule = new int[num_obj_create_rule];
                    for (int j = 0; j < num_obj_create_rule; j++)
                    {
                        obj_for_rule[j] = rand() % train_length;
                    }
                    create_rule(num_term, columnNumber, train_data, in_rule, obj_for_rule, num_obj_create_rule);

                    /*for (int j = 0; j < columnNumber; j++)
                    {
                        cout << rule[j] << " ";
                    }*/

                    for (int j = 0; j < num_class; j++)
                    {
                        in_confid[j] = 0;
                    }
                    
                    confidence(num_class, train_length, columnNumber, class_answers, data, in_rule, in_confid);
                    /*
                    for (int j = 0; j < num_class; j++)
                    {
                        cout << in_confid[j] << " ";
                    }*/

                    /*cout << endl << " Правило " << endl;
                    for (int j = 0; j < columnNumber; j++)
                    {
                        cout << in_rule[j] << " ";
                    }*/

                    double max_confid = 0;
                    int c_confid = 0;
                    for (int j = 0; j < num_class; j++)
                    {
                        if (in_confid[j] > max_confid)
                        {
                            max_confid = in_confid[j];
                            c_confid = j;
                        }
                    }

                    if (max_confid > better_than)
                    {
                        flag++;
                        for (int y = 0; y < columnNumber; y++)
                        {
                            pop[ipop][q][y] = in_rule[y];
                        }
                        class_rules[0][ipop][q] = c_confid;
                        confid_rules[0][ipop][q] = max_confid;
                        weight_rules[0][ipop][q] = 2*max_confid - 1;
                        active_rules[0][ipop][q] = 1;
                    }
                    else
                    {
                        for (int y = 0; y < columnNumber; y++)
                        {
                            pop[ipop][q][y] = 0;
                        }
                        active_rules[0][ipop][q] = 0;
                        class_rules[0][ipop][q] = 0;
                        confid_rules[0][ipop][q] = 0;
                        weight_rules[0][ipop][q] = 0;
                    }

                    delete[] obj_for_rule;
                }
                if (flag < num_class)
                {
                    for (int q = 0; q < number_rules; q++)
                    {
                        if (active_rules[0][ipop][q] == 0)
                        {
                            //заменяем его на то, которое подходит
                            for (int j = 0; j < columnNumber; j++)
                            {
                                in_rule[j] = 0;
                            }
                            //случайный объект из выборки
                            int random_obj = rand() % train_length;
                            int* obj_for_rule = new int[num_obj_create_rule];
                            for (int j = 0; j < num_obj_create_rule; j++)
                            {
                                obj_for_rule[j] = rand() % train_length;
                            }
                            create_rule(num_term, columnNumber, train_data, in_rule, obj_for_rule, num_obj_create_rule);

                            for (int j = 0; j < num_class; j++)
                            {
                                in_confid[j] = 0;
                            }

                            confidence(num_class, train_length, columnNumber, class_answers, data, in_rule, in_confid);

                            /*for (int j = 0; j < num_class; j++)
                            {
                                cout << in_confid[j] << " ";
                            }*/

                            double max_confid = 0;
                            int c_confid = 0;
                            for (int j = 0; j < num_class; j++)
                            {
                                if (in_confid[j] > max_confid)
                                {
                                    max_confid = in_confid[j];
                                    c_confid = j;
                                }
                            }

                            if (max_confid > better_than)
                            {
                                flag++;
                                for (int y = 0; y < columnNumber; y++)
                                {
                                    pop[ipop][q][y] = in_rule[y];
                                }
                                class_rules[0][ipop][q] = c_confid;
                                confid_rules[0][ipop][q] = max_confid;
                                weight_rules[0][ipop][q] = 2*max_confid - 1;
                                active_rules[0][ipop][q] = 1;
                            }

                            else
                            {
                                for (int y = 0; y < columnNumber; y++)
                                {
                                    pop[ipop][q][y] = 0;
                                }
                                active_rules[0][ipop][q] = 0;
                                class_rules[0][ipop][q] = 0;
                                confid_rules[0][ipop][q] = 0;
                                weight_rules[0][ipop][q] = 0;
                            }
                            delete[] obj_for_rule;
                        }
                        if (flag > num_class)
                        {
                            break;
                        }
                    }
                }
                /*
                for (int j = 0; j < number_rules; j++)
                {
                    for (int y = 0; y < columnNumber; y++)
                    {
                        cout << pop[i][j][y] << " ";
                    }
                    cout << endl;
                }
                
                for (int y = 0; y < number_rules; y++)
                {
                    cout << class_rules[0][i][y] << " ";
                    if (active_rules[0][i][y] == 0)
                    {
                        cout << "<-wrong ";
                    }
                }*/
                /*for (int y = 0; y < number_rules; y++)
                {
                    cout << "active " << active_rules[0][i][y] << endl;
                    cout << "confidence " << confid_rules[0][i][y] << endl;
                    cout << "weight " << weight_rules[0][i][y] << endl;
                }
                cout << endl;*/
            }
        }
        else 
        {
            for (int ipop = 0; ipop < pop_size; ipop++)
            {
                //задание изначального количества для популяции
                if (columnNumber <= number_rules/2)
                {
                    q_number = columnNumber;
                }
                else
                {
                    q_number = number_rules/2;
                }
                    
                int flag = 0;
                int* random_object = new int[num_random];
                int* obj_for_rule = new int[num_obj_create_rule];
                for (int j = 0; j < num_obj_create_rule; j++)
                {
                    obj_for_rule[j] = 0;
                }
                //заполнение правил
                for (int q = 0; q < q_number; q++)
                {
                    int* in_rule = new int[columnNumber];
                    for (int j = 0; j < columnNumber; j++)
                    {
                        in_rule[j] = 0;
                    }
                    int random_class = -1;
                    random_class = rand() % num_class;
                    //массив с num_random случайными объектами из выборки одного класса
                    random_object[0] = rand() % train_length;
                    while (train_class_answers[random_object[0]] != random_class)
                    {
                        random_object[0] = rand() % train_length;
                    }
                    //случайный объект из выборки
                    int random_obj = 0;
                    int answer_class_check = train_class_answers[random_object[0]];
                    int count = 1;
                    for (int ran = 1; ran < num_random; ran++)
                    {
                        random_obj = rand() % train_length;
                        while (train_class_answers[random_obj] != train_class_answers[random_object[0]])
                        {
                            random_obj = rand() % train_length;
                        }
                        int check_same = 0;
                        for (int j = 0; j < count; j++)
                        {
                            if (random_obj == random_object[j])
                            {
                                check_same = 1;
                            }
                        }
                        if (check_same == 0)
                        {
                            random_object[ran] = random_obj;
                            count++;
                        }
                        else 
                        {
                            random_obj = rand() % train_length;
                            while ((train_class_answers[random_obj] != train_class_answers[random_object[0]]))
                            {
                                random_obj = rand() % train_length;
                                count++;
                            }
                            random_object[ran] = random_obj;
                        }
                    }

                    /*for (int j = 0; j < num_random; j++)
                    {
                        cout << random_object[j] << " ";
                    }*/

                    int ed_fact = 0;
                    ed_fact = num_random - 1;
                    double** edist = new double*[ed_fact];
                    for (int e = 0; e < ed_fact; e++)
                    {
                        edist[e] = new double[2];
                    }

                    for (int j = 0; j < ed_fact; j++)
                    {
                        for (int j1 = 0; j1 < 2; j1++)
                        {
                            edist[j][j1] = 0;
                        }
                    }

                    //обработка массива с выбранными данными из одного класса
                    int dcount = 0;
                    int edist_min = 100;
                    for (int ran = 0; ran < num_random; ran++)
                    {
                        if (train_data[random_object[ran]] != train_data[random_object[0]])
                        {
                            edist[dcount][0] = EuclideanDistance(train_data[random_object[0]], train_data[random_object[ran]], columnNumber);
                            edist[dcount][1] = random_object[ran];
                            dcount++;
                        }
                    } 

                    /*cout << endl << " Еdist before" << endl;
                    for (int j = 0; j < 2; j++)
                    {
                        for (int j1 = 0; j1 < ed_fact; j1++)
                        {
                            cout << edist[j1][j] << " ";
                        }
                        cout << endl;
                    }*/

                    for(int r = 0; r < ed_fact-1; r++)
                    {
                        // Поиск наименьшего в первом столбце
                        double m = edist[r][0];
                        int idx = r;
                        for(int l = r; l < ed_fact; l++)
                        {
                            if (edist[l][0] < m) 
                            {
                                m = edist[l][0];
                                idx = l;
                                // Обмен
                                swap_rows(edist, r, idx);
                            }
                        }                    
                    }

                    /*cout << endl << " Еdist after" << endl;
                    for (int j = 0; j < 2; j++)
                    {
                        for (int j1 = 0; j1 < ed_fact; j1++)
                        {
                            cout << edist[j1][j] << " ";
                        }
                        cout << endl;
                    }*/

                    obj_for_rule[0] = random_object[0];
                    int count_for_inserting = 0;
                    for (int j = 1; j < num_obj_create_rule; j++)
                    {
                        obj_for_rule[j] = edist[count_for_inserting][1];
                        count_for_inserting++;
                    }

                    //найти ближкий объект другого класса и попробовать не брать его термы
                    //а должны ли все термы быть не такими же, как из другого класса?..
                    create_rule(num_term, columnNumber, train_data, in_rule, obj_for_rule, num_obj_create_rule);

                    //теперь нужно понять насколько хорошо правило, если норм - то оставляем
                    for (int j = 0; j < num_class; j++)
                    {
                        in_confid[j] = 0;
                    }
                    
                    confidence(num_class, train_length, columnNumber, class_answers, data, in_rule, in_confid);
                    
                    /*cout << endl << " Следующее " << endl;
                    for (int j = 0; j < num_class; j++)
                    {
                        cout << in_confid[j] << " ";
                    }*/

                    /*
                    cout << endl << " Правило " << endl;
                    for (int j = 0; j < columnNumber; j++)
                    {
                        cout << in_rule[j] << " ";
                    }*/

                    double max_confid = 0;
                    int c_confid = 0;
                    for (int j = 0; j < num_class; j++)
                    {
                        if (in_confid[j] > max_confid)
                        {
                            max_confid = in_confid[j];
                            c_confid = j;
                        }
                    }

                    if (answer_class_check == c_confid && max_confid > better_than)
                    { //проверить вот это работу, подумать
                        flag++;
                        for (int y = 0; y < columnNumber; y++)
                        {
                            pop[ipop][q][y] = in_rule[y];
                        }
                        class_rules[0][ipop][q] = c_confid;
                        confid_rules[0][ipop][q] = max_confid;
                        weight_rules[0][ipop][q] = 2*max_confid - 1;
                        active_rules[0][ipop][q] = 1;
                    }
                    else
                    {
                        for (int y = 0; y < columnNumber; y++)
                        {
                            pop[ipop][q][y] = 0;
                        }
                        active_rules[0][ipop][q] = 0;
                        class_rules[0][ipop][q] = 0;
                        confid_rules[0][ipop][q] = 0;
                        weight_rules[0][ipop][q] = 0;
                    }

                    for (int j = 0; j < ed_fact; j++)
                    {
                        delete edist[j];
                    }
                    delete edist;
                }
                
                //проверка на количество правил
                //если меньше минимального, то дозаполнить
                if (flag < num_class)
                {
                    for (int q = 0; q < number_rules; q++)
                    {
                        if (active_rules[0][ipop][q] == 0)
                        {
                            //заменяем его на то, которое подходит
                            for (int j = 0; j < columnNumber; j++)
                            {
                                in_rule[j] = 0;
                            }
                            //случайный объект из выборки
                            int random_obj = rand() % train_length;
                            int* obj_for_rule = new int[num_obj_create_rule];
                            for (int j = 0; j < num_obj_create_rule; j++)
                            {
                                obj_for_rule[j] = rand() % train_length;
                            }
                            create_rule(num_term, columnNumber, train_data, in_rule, obj_for_rule, num_obj_create_rule);

                            for (int j = 0; j < num_class; j++)
                            {
                                in_confid[j] = 0;
                            }

                            confidence(num_class, train_length, columnNumber, class_answers, data, in_rule, in_confid);

                            /*for (int j = 0; j < num_class; j++)
                            {
                                cout << in_confid[j] << " ";
                            }*/

                            double max_confid = 0;
                            int c_confid = 0;
                            for (int j = 0; j < num_class; j++)
                            {
                                if (in_confid[j] > max_confid)
                                {
                                    max_confid = in_confid[j];
                                    c_confid = j;
                                }
                            }

                            if (max_confid > better_than)
                            {
                                flag++;
                                for (int y = 0; y < columnNumber; y++)
                                {
                                    pop[ipop][q][y] = in_rule[y];
                                }
                                class_rules[0][ipop][q] = c_confid;
                                confid_rules[0][ipop][q] = max_confid;
                                weight_rules[0][ipop][q] = 2*max_confid - 1;
                                active_rules[0][ipop][q] = 1;
                            }

                            else
                            {
                                for (int y = 0; y < columnNumber; y++)
                                {
                                    pop[ipop][q][y] = 0;
                                }
                                active_rules[0][ipop][q] = 0;
                                class_rules[0][ipop][q] = 0;
                                confid_rules[0][ipop][q] = 0;
                                weight_rules[0][ipop][q] = 0;
                            }
                        }
                        if (flag > num_class)
                        {
                            break;
                        }
                    }
                }

                /*for (int j = 0; j < number_rules; j++)
                {
                    for (int y = 0; y < columnNumber; y++)
                    {
                        cout << pop[ipop][j][y] << " ";
                    }
                    cout << endl;
                }*/

                delete[] random_object;
                delete[] obj_for_rule;
            }
        }

        int** best_rule_base = new int*[number_rules];
        for (int j = 0; j < number_rules; j++)
        {
            best_rule_base[j] = new int[columnNumber];
        }

        //Обнуление
        for (int j = 0; j < number_rules; j++)
        {
            for (int l = 0; l < columnNumber; l++)
            {
                best_rule_base[j][l] = 0;
            }
        }

        int* best_class_rule_base = new int[number_rules];
        double* best_confid_rule_base = new double[number_rules];
        int* best_active_rule_base = new int[number_rules];
        

        //Обнуление
        for (int j = 0; j < number_rules; j++)
        {
            best_class_rule_base[j] = 0;
            best_confid_rule_base[j] = 0;
            best_active_rule_base[j] = 0;
        } 

        double best_fitness = -1;
        double best_percentage = lineNumber;

        //cout << "Generation " << generation << endl;
        double** reply_train = new double*[pop_size];
        for (int j = 0; j < pop_size; j++)
        {
            reply_train[j] = new double[train_length];
        }

        double** reply_test = new double*[pop_size];
        for (int j = 0; j < pop_size; j++)
        {
            reply_test[j] = new double[test_length];
        }

        //Обнуление
        for (int j = 0; j < pop_size; j++)
        {
            for (int l = 0; l < train_length; l++)
            {
                reply_train[j][l] = 0;
            }
            for (int l = 0; l < test_length; l++)
            {
                reply_test[j][l] = 0;
            }
        }

        double* rank = new double[pop_size];
        for (int j = 0; j < pop_size; j++)
        {
            rank[j] = j + 1;
        }

        double error_percentage_train = 0;
        double error_percentage_test = 0;
        int best_index = 0;
        //отправить на проверку обучающую выборку
        for (int y = 0; y < pop_size; y++)
        {
            for (int j = 0; j < train_length; j++)
            {
                reply_train[y][j] = 0;
            }

            for (int j = 0; j < train_length; j++)
            {
                Rules(train_data[j], pop[y], class_rules[0][y], confid_rules[0][y], columnNumber, number_rules, best_rule_for_object_train, reply_train, y, j);//отправка обучающей выборки
            }

            /*for (int j = 0; j < train_length; j++)
            {
                cout << reply_train[y][j] << endl;
            }*/

            didntgetclass = 0;

            int train_error = 0;
            for (int l = 0; l < train_length; l++)
            {
                if (reply_train[y][l] == -1)
                {
                    didntgetclass++;
                }
                if (reply_train[y][l] != train_class_answers[l])
                {
                    train_error = train_error + 1;//неправильно классифицированный объекты
                    correct_classification_for_object_train[y][l] = 0;//неправильно классифицированный объект
                }
                else
                {
                    correct_classification_for_object_train[y][l] = 1;//правильно классифицированный объект
                }
            }
            //количество правильно классиф. объектов для каждого правила
            check_fitness_michegan(correct_classification_for_object_train[y], best_rule_for_object_train[y], y, number_rules, (lineNumber-(cross_num+last_data)), correct_classification_num); 
            
            for (int j = 0; j < number_rules; j++)
            {
                if (confid_rules[0][y][j] < better_than)
                {
                    active_rules[0][y][j] = 0;
                    class_rules[0][y][j] = 0;
                    confid_rules[0][y][j] = 0;
                    weight_rules[0][y][j] = 0;
                }

                if (correct_classification_num[y][j] == 0)
                {
                    active_rules[0][y][j] = 0;
                    class_rules[0][y][j] = 0;
                    confid_rules[0][y][j] = 0;
                    weight_rules[0][y][j] = 0;
                }
            }
            
            int flag_active = 0;
            flag_active = active_rule_flag(confid_rules[0][y], number_rules, better_than);

            double flag_not_dontcare = 0;
            flag_not_dontcare = dont_care_flag(number_rules, columnNumber, pop[y], confid_rules[0][y], better_than);
            
            //cout << "train error " << train_error << " out of " << train_length << " with " << flag_active << endl;

            int** mfalse = new int* [num_class+1]; 
            for (int j = 0; j < num_class+1; j++)
            {
                mfalse[j] = new int[num_class+1];
            }

            //Обнуление
            for (int j = 0; j < num_class+1; j++)
            {
                for (int l = 0; l < num_class+1; l++)
                {
                    mfalse[j][l] = 0;
                }
            }

            //матрица ошибок для задач с N классами
            error_matrix(train_length, reply_train[y], train_class_answers, class_value, num_class, class_values_count_train, mfalse, didntgetclass);

            //cout << didntgetclass;

            accuracy = 0;
            precision = 0;
            recall = 0;
            Fscore = 0;
            numerator = 0;
            denominator = 0; 
            b = 1;

            /*for (int j = 0; j < num_class; j++)
            {
                for (int l = 0; l < num_class; l++)
                {
                    cout << mfalse[j][l] << " ";
                }
                cout << endl;
            }*/

            for (int j = 0; j < num_class; j++)
            {
                int sum_pre = 0;
                int class_pre = 0;
                int class_recall = 0;
                int sum_recall = 0;
                for (int l = 0; l < num_class; l++)
                {
                    if (j == l)
                    {
                        numerator = numerator + mfalse[j][l];
                        class_pre = mfalse[j][l];
                        class_recall = mfalse[l][j];
                    }
                    denominator = denominator + mfalse[j][l];

                    sum_pre = sum_pre + mfalse[j][l];
                    sum_recall = sum_recall + mfalse[l][j];
                    
                }
                if (sum_recall != 0)
                {
                    recall = recall + (double)class_recall/(double)sum_recall;
                }
                if (sum_pre != 0)
                {
                    precision = precision + (double)class_pre/(double)sum_pre;
                }
            }

            accuracy = (double)numerator/(double)denominator;//для любого кол-ва классов
            best_accuracy = accuracy;
            precision = precision/(double)num_class;//для любого кол-ва классов
            recall = recall/(double)num_class;//для любого кол-ва классов
            if (precision + recall == 0)
            {
                Fscore = 0;
            }
            else
            {
                Fscore = (b * b + 1) * (precision*recall / (b * b* (precision + recall)));//для любого кол-ва классов
            }

            num_rule_file = flag_active;
            best_index = y;
            dont_care_file = dont_care_flag(number_rules, columnNumber, pop3[best_index], confid_rules[0][y], better_than);

            /*cout << endl << " Accurancy " << accuracy << endl;//Общая точность классификации
            cout << endl << " Precision " << precision << endl;//Согласованность классификации первого класса с данными
            cout << endl << " Recall (Sensitivity) " << recall << endl;//Эффективность классификатора по выделению первого класса
            cout << endl << " Fscore " << Fscore << endl;//Отношение между объектами первого класса в данных и предсказанными классификатором
            */
            
            for (int j = 0; j < num_class+1; j++)
            {
                delete mfalse[j];
            }
            delete mfalse;

            error_percentage_train = double(train_error) / double(train_length);
            //f3 количество не донт care параметров в правиле суммарно по всем правилам 
            //реализовать NSGA-II
            fitness_small[y] = error_percentage_train;
            f_score_fit[y] = (1 - Fscore);
            accuracy_fit[y] = accuracy;
            num_rules_fit[y] = flag_active;
            fitness[y] = w1*(1 - Fscore) + w2*flag_active + w3*flag_not_dontcare;//оптимизировать
            
            //cout << "Train fitness " << fitness[y] << endl;


            if (fitness[y] < best_fitness || best_fitness == -1)
            {

                for (int l = 0; l < number_rules; l++)
                {
                    for (int j = 0; j < columnNumber; j++)
                    {
                        best_rule_base[l][j] = pop[y][l][j];//база правил с наименьшей ошибкой
                    }
                    best_confid_rule_base[l] = confid_rules[0][y][l];
                    best_active_rule_base[l] = active_rules[0][y][l];
                    best_class_rule_base[l] = class_rules[0][y][l];
                }

                /*for (int l = 0; l < number_rules; l++)
                {
                    for (int j = 0; j < columnNumber; j++)
                    {
                        cout << best_rule_base[l][j] << "\t";
                    }
                    cout << best_class_rule_base[l] << " " << best_active_rule_base[l] << " " << best_confid_rule_base[l] << endl;
                }
                cout << endl;*/

                //cout << "Train percentage " << error_percentage_train << endl;

                best_fitness = fitness[y];
                best_percentage = error_percentage_train;
                best_index = y;

            }          
        }

        //Старт 
        for (int generation = 0; generation < gen; generation++)
        {
            //селекция              
            if (which_selection == 0)
            {
                //Ранговая нелинейные ранги
                //отправляем в функцию+
                //в функции
                //сортируем массив по возрастанию fitness+
                //ставим ранги, проверяем одинаковые fitness+
                //для одинаковых средний ранг рассчитываем+- ТОЛЬКО ДЛЯ 2-х повторяющихся
                //возвращаем все ранги и фитнес на место в main, вроде +
                //ну и по рангу смотрим какой лучше взять +
                rank_selection(fitness, pop_size, rank);

                double sum_rang = 0;

                for (int j = 0; j < pop_size; j++)
                {
                    //cout << rank[j] << " - " << fitness[j] << endl;
                    sum_rang = sum_rang + rank[j]; 
                }

                for (int j = 0; j < pop_size; j++)
                {
                    fitness_rang[j] = rank[j]/sum_rang;
                }

                double sum_pred = 0;
                for (int j = 0; j < pop_size; j++)
                {
                    sum_pred = sum_pred + fitness_rang[j];
                    fitness_rang[j] = sum_pred;
                }
                
                /*for (int l = 0; l < pop_size-1; l++)
                {
                    // Поиск наименьшего в первом столбце
                    double m = prop_sel[l][0];
                    int idx = l;
                    for(int j = l; j < pop_size; j++)
                    {
                        if (prop_sel[j][0] < m) 
                        {
                            m = prop_sel[j][0];

                            double a1 = prop_sel[j][0];
                            prop_sel[j][0] = prop_sel[l][0];
                            prop_sel[l][0] = a1;

                            double a2 = prop_sel[j][1];
                            prop_sel[j][1] = prop_sel[l][1];
                            prop_sel[l][1] = a2;
                        }
                    }
                }*/

                //вроде бы так, уточнить
                for (int j = 0; j < pop_size; j++)
                {
                    double k = xrand(0, 1);
                    for (int j1 = 0; j1 < pop_size; j1++)
                    {                        
                        if (k <= fitness_rang[j1])
                        {
                            for (int l = 0; l < number_rules; l++)
                            {
                                for (int p = 0; p < columnNumber; p++)
                                {
                                    pop2[j][l][p] = pop[j1][l][p];
                                }
                                rules_update[1][j][l] = 1;
                                class_rules[1][j][l] = class_rules[0][j][l];
                                active_rules[1][j][l] = active_rules[0][j][l];
                                confid_rules[1][j][l] = confid_rules[0][j][l];
                                weight_rules[1][j][l] = weight_rules[0][j][l];
                            }
                            break;
                        }
                    }
                }
            }
            else 
            {
                //Турнирная с N размером турнира
                int* dry = new int[T];
                int selection_index = 0;
                int randpop = 0;
                for (int y = 0; y < pop_size; y++)
                {
                    for (int u = 0; u < T; u++)
                    {
                        if (u == 0)
                        {
                            //только в турнире не повторялись
                            randpop = rand() % pop_size;
                            dry[u] = randpop;
                        }
                        else
                        {
                            int flag_same = 1;
                            while (flag_same == 1)
                            {
                                randpop = rand() % pop_size;
                                flag_same = 0;
                                for (int y2 = 0; y2 < u; y2++)
                                {
                                    if (randpop == dry[y2])
                                    {
                                        flag_same = 1;
                                    }
                                }
                            }
                            dry[u] = randpop;
                        }
                        if (fitness[randpop] < fitness[selection_index] || u == 0)
                        {
                            selection_index = randpop;
                        }
                    }
                    for (int l = 0; l < number_rules; l++)
                    {
                        for (int j = 0; j < columnNumber; j++)
                        {
                            pop2[y][l][j] = pop[selection_index][l][j];
                        }
                        rules_update[1][y][l] = 1;
                        class_rules[1][y][l] = class_rules[0][selection_index][l];
                        active_rules[1][y][l] = active_rules[0][selection_index][l];
                        confid_rules[1][y][l] = confid_rules[0][selection_index][l];
                        weight_rules[1][y][l] = weight_rules[0][selection_index][l];
                    }
                }

                delete[] dry;
            }

            //скрещивание
            if (which_crossover == 0)
            {
                //равномерное
                /*
                Избежать плохое скрещивание:
                (Берем среднее и генерируем по нормальному закону с какой-нибудь дисперсией вокруг среднего)
                Сколько каждое из правил классифицировало объектов ->\
                Брать чаще те правила, у которых больше
                */
                int selection_index = 0;
                int randpop = 0;
                for (int y = 0; y < pop_size; y++)
                {
                    //не запоминаю, какие индивиды были, а может надо бы!
                    int k = rand() % pop_size;
                    while (y == k)
                    {
                        k = rand() % pop_size;
                    }
                    int new_num_rules = 0;
                    int flag_active1 = active_rule_flag(confid_rules[1][y], number_rules, better_than);
                    int flag_active2 = active_rule_flag(confid_rules[1][k], number_rules, better_than);
                    if (flag_active1 + flag_active2 - num_class <= 0)
                    {
                        new_num_rules = flag_active1 + flag_active2;
                    }
                    else
                    {
                        new_num_rules = rand() % (flag_active1 + flag_active2 - num_class) + num_class;
                    }
                    
                    int* taken_rules1 = new int[number_rules];
                    int* taken_rules2 = new int[number_rules];

                   
                    if (new_num_rules > number_rules)
                    {
                        new_num_rules = number_rules;
                    }

                    //обнуление
                    for (int l = 0; l < number_rules; l++)
                    {
                        taken_rules1[l] = 0;
                        taken_rules2[l] = 0;
                    }
                    
                    for (int l = 0; l < new_num_rules; l++)
                    {
                        double k1 = xrand(0, 1);
                        if (flag_active1 <= 0)
                        {
                            k1 = 0.7;
                        }
                        if (flag_active2 <= 0)
                        {
                            k1 = 0.2;
                        }
                        if (k1 < 0.5)
                        {
                            int l1 = rand() % number_rules;
                            int outofwhile = 0;
                            while (outofwhile < number_rules + 1)
                            {
                                //если в этом индивиде кончатся правила, то нужно брать из другого
                                if (active_rules[1][y][l1] == 1 && taken_rules1[l1] == 0)//искать активное!
                                {
                                    taken_rules1[l1] = 1;
                                    for (int j = 0; j < columnNumber; j++)
                                    {
                                        pop3[y][l][j] = pop2[y][l1][j];
                                    }
                                    active_rules[2][y][l] = active_rules[1][y][l1];
                                    class_rules[2][y][l] = class_rules[1][y][l1];
                                    confid_rules[2][y][l] = confid_rules[1][y][l1];
                                    weight_rules[2][y][l] = weight_rules[1][y][l1];
                                    flag_active1--;
                                    break;
                                }
                                l1++;
                                outofwhile++;
                                l1 = l1 % number_rules;
                            }
                        }
                        else
                        {
                            int l1 = rand() % number_rules;
                            int outofwhile = 0;
                            while (outofwhile < number_rules + 1)
                            {
                                //если в этом индивиде кончатся правила, то нужно брать из другого
                                if (active_rules[1][k][l1] == 1 && taken_rules2[l1] == 0)//искать активное!
                                {
                                    taken_rules2[l1] = 1;
                                    for (int j = 0; j < columnNumber; j++)
                                    {
                                        pop3[y][l][j] = pop2[k][l1][j];
                                    }
                                    active_rules[2][y][l] = active_rules[1][k][l1];
                                    class_rules[2][y][l] = class_rules[1][k][l1];
                                    confid_rules[2][y][l] = confid_rules[1][k][l1];
                                    weight_rules[2][y][l] = weight_rules[1][k][l1];
                                    flag_active2--;
                                    break;
                                }
                                l1++;
                                outofwhile++;
                                l1 = l1 % number_rules;
                            }
                        }
                    }
                    delete[] taken_rules1;
                    delete[] taken_rules2;

                    int start = 0;
                    int count_class_cross = num_class;
                    int flag_active3 = active_rule_flag(confid_rules[2][y], number_rules, better_than);
                    //больше ли num_class или нет проверку везде сделать!ВАЖНО
                } 
            }
            else
            {
                //Новая с кол-вом верно классиф.объектов
                /*
                Избежать плохое скрещивание:
                (Берем среднее и генерируем по нормальному закону с какой-нибудь дисперсией вокруг среднего)
                Сколько каждое из правил классифицировало объектов ->\
                Брать чаще те правила, у которых больше
                */
                int selection_index = 0;
                int randpop = 0;

                for (int y = 0; y < pop_size; y++)
                {
                    //не запоминаю, какие индивиды были, а надо бы!
                    int k = rand() % pop_size;
                    while (y == k)
                    {
                        k = rand() % pop_size;
                    }
                    
                    int flag_active1 = active_rule_flag(confid_rules[1][y], number_rules, better_than);
                    int flag_active2 = active_rule_flag(confid_rules[1][k], number_rules, better_than);
                    
                    int new_num_rules = 0;

                    if (flag_active1 + flag_active2 - num_class <= 0)
                    {
                        new_num_rules = flag_active1 + flag_active2;
                    }
                    else
                    {
                        new_num_rules = rand() % (flag_active1 + flag_active2 - num_class) + num_class;
                    }

                    if (new_num_rules > number_rules)
                    {
                        new_num_rules = number_rules;
                    }

                    checkz_fitness_michegan(correct_classification_for_object_train[y], best_rule_for_object_train[y], y, number_rules, (lineNumber-(cross_num+last_data)), fitness_michegan);
                    checkz_fitness_michegan(correct_classification_for_object_train[k], best_rule_for_object_train[k], k, number_rules, (lineNumber-(cross_num+last_data)), fitness_michegan2);

                    for (int l = 0; l < new_num_rules; l++)
                    {
                        int rule1 = rand() % number_rules;//активное из y
                        int rule2 = rand() % number_rules;//активное из k

                        for (int l1 = 0; l1 < number_rules; l1++)
                        {
                            if (confid_rules[1][y][rule1] < better_than)
                            {
                                rule1++;
                                rule1 = rule1%number_rules;
                            }
                            if (confid_rules[1][k][rule2] < better_than)
                            {
                                rule2++;
                                rule2 = rule2%number_rules;
                            }
                        }
                      
                        if (fitness_michegan[y][rule1] > fitness_michegan2[k][rule2])
                        {
                            for (int j = 0; j < columnNumber; j++)
                            {
                                pop3[y][l][j] = pop2[y][rule1][j];
                            }
                            active_rules[2][y][l] = active_rules[1][y][rule1];
                            class_rules[2][y][l] = class_rules[1][y][rule1];
                            confid_rules[2][y][l] = confid_rules[1][y][rule1];
                            weight_rules[2][y][l] = weight_rules[1][y][rule1];
                        }
                        else 
                        {
                            for (int j = 0; j < columnNumber; j++)
                            {
                                pop3[y][l][j] = pop2[k][rule2][j];
                            }
                            active_rules[2][y][l] = active_rules[1][k][rule2];
                            class_rules[2][y][l] = class_rules[1][k][rule2];
                            confid_rules[2][y][l] = confid_rules[1][k][rule2];
                            weight_rules[2][y][l] = weight_rules[1][k][rule2];
                        }
                    }
                } 
            }

            //мутация
            //терм + сгенерированное число от 1 до 13 и остаток от деления на 14
            for (int y = 0; y < pop_size; y++)
            {
                int flag_active = active_rule_flag(confid_rules[2][y], number_rules, better_than);
                int flag_confid = confid_rule_flag(confid_rules[2][y], number_rules, better_than);
                double k1 = 0;
                if (which_mutation == 0)
                {
                    //переменные / 3 
                    k1 = 1.0 / double(columnNumber) / double(flag_confid) / 3.0;//слабая
                }
                else if (which_mutation == 1)
                {
                    k1 = 1.0 / double(columnNumber) / double(flag_confid);//средняя 
                    
                }
                else 
                {
                    double var_min = 3.0 / double(columnNumber) / double(flag_confid);
                    k1 = min(var_min, 1.0);//сильная 
                }
                for (int l = 0; l < number_rules; l++)
                {
                    for (int j = 0; j < columnNumber; j++)
                    {
                        int k4 = (rand() % (num_term-2)) + 1;
                        int new_term = (pop3[y][l][j] + k4) % num_term;
                        double k = xrand(0, 1);
                        if (k < k1)
                        {
                            pop3[y][l][j] = new_term;
                        }
                    }
                }
            }

//-------------------------------------------Мичиганская часть----------------------------------------------------
            int selection_index = 0;
            int selection_index1 = 0;
            int randrul = 0;

            //селекция 
            //пригодность правила - это количество объектов которые это конкретное правило классифицировало

            for (int y = 0; y < pop_size; y++)
            {
                int new_number_rules_mich_ga = 0;
                int new_number_rules_mich_h = 0;

                checkz_fitness_michegan(correct_classification_for_object_train[y], best_rule_for_object_train[y], y, number_rules, (lineNumber-(cross_num+last_data)), fitness_michegan);
                //так как здесь мы работаем с правилами, то замена только активных с вероятность 50/50 как га, так и эвристикой
                int flag_active = active_rule_flag(confid_rules[2][y], number_rules, better_than);
                int check_for_zero = 0;
                if (flag_active == 0)
                {
                    flag_active = 1;
                    check_for_zero = 1;
                }
                int new_rule_mich = (float)flag_active/5.0 + 2.0;

                if (new_rule_mich % 2 == 0)     
                {
                    new_number_rules_mich_ga =  new_rule_mich >> 1;
                    new_number_rules_mich_h =  new_rule_mich >> 1;
                }
                else if (new_rule_mich != 1)
                {
                    double rand_num = xrand(0, 1);
                    if (rand_num <= 0.6)//эвристика
                    {
                        new_number_rules_mich_h = new_rule_mich >> 1; 
                        new_number_rules_mich_ga = new_rule_mich - new_number_rules_mich_h;
                    }
                    else//га
                    {
                        new_number_rules_mich_ga = new_rule_mich >> 1;
                        new_number_rules_mich_h = new_rule_mich - new_number_rules_mich_ga;
                    }
                }
                else if (new_rule_mich == 1)
                {
                    double rand_num = xrand(0, 1);
                    if (rand_num <= 0.6)//эвристика
                    {
                        new_number_rules_mich_h = 1;
                    }
                    else//га
                    {
                        new_number_rules_mich_ga = 1;
                    }
                }      
                //cout << y << " " << new_number_rules_mich_ga << " " << new_number_rules_mich_h << endl;
                //селекция                 
                for (int l = 0; l < new_number_rules_mich_ga; l++)
                {
                    selection_index = 0;
                    randrul = rand() % (number_rules);
                    while (active_rules[2][y][randrul] == 0)
                    {
                        randrul = rand() % (number_rules);
                    }
                    selection_index = randrul;
                    for (int u = 0; u < T; u++)
                    {
                        randrul = rand() % (number_rules);
                        while (active_rules[2][y][randrul] == 0)
                        {
                            randrul = rand() % (number_rules);
                        }
                        
                        if (fitness_michegan[y][randrul] > fitness_michegan[y][selection_index] || u == 0)//корректно классифицировал
                        {
                            selection_index1 = selection_index;
                            selection_index = randrul;
                        }
                    }
                    //мутировать с скрещиванием
                    double mut = 1.0 / (double)columnNumber;
                    for (int j = 0; j < columnNumber; j++)
                    {
                        double k = xrand(0, 1);
                        if (k <= 0.5)
                        {
                            out[y][l][j] = pop3[y][selection_index][j];
                        }
                        else
                        {
                            out[y][l][j] = pop3[y][selection_index1][j];
                        }
                        if (mut > k)
                        {
                            int k4 = (rand() % (num_term-2)) + 1;
                            //терм + сгенерированное число от 1 до 13 и остаток от деления на 14  
                            int new_term = (out[y][l][j] + k4) % num_term;
                            out[y][l][j] = new_term;
                        }
                    }
                }

                for (int l = 0; l < number_rules; l++)
                {
                    if (confid_rules[2][y][l] < better_than)
                    {
                        int bad_obj = 0;
                        for (int linenum = 0; linenum < train_length; linenum++)
                        {
                            int check = 0;
                            if (correct_classification_for_object_train[y][linenum] == 0)
                            {
                                bad_obj = linenum;
                                for (int j = 0; j < columnNumber; j++)
                                {
                                    in_rule[j] = 0;
                                }
                                //случайный объект из выборки
                                int* obj_for_rule = new int[num_obj_create_rule];
                                obj_for_rule[0] = bad_obj;
                                for (int j = 1; j < num_obj_create_rule; j++)
                                {
                                    obj_for_rule[j] = rand() % train_length;
                                    //генерировать случайную позицию, затем до конца, а потом сначала до первой сгенерированной изменить while (от 0 колва активных правил)
                                    for (int jjj = 0; jjj < train_length >> 1; jjj++)
                                    {
                                        obj_for_rule[j] = rand() % train_length;
                                        if (correct_classification_for_object_train[y][obj_for_rule[j]] == 0)
                                        {
                                            break;
                                        }
                                    }
                                }
                                create_rule(num_term, columnNumber, train_data, in_rule, obj_for_rule, num_obj_create_rule);

                                for (int j = 0; j < num_class; j++)
                                {
                                    in_confid[j] = 0;
                                }
                                
                                confidence(num_class, lineNumber, columnNumber, class_answers, data, in_rule, in_confid);
                                
                                double max_confid = 0;
                                int c_confid = 0;
                                for (int j = 0; j < num_class; j++)
                                {
                                    if (in_confid[j] > max_confid)
                                    {
                                        max_confid = in_confid[j];
                                        c_confid = j;
                                    }
                                }

                                if (max_confid > better_than)
                                {
                                    new_number_rules_mich_h--;
                                    check = 1;
                                    for (int q = 0; q < columnNumber; q++)
                                    {
                                        pop3[y][l][q] = in_rule[q];
                                    }
                                    class_rules[2][y][l] = c_confid;
                                    confid_rules[2][y][l] = max_confid;
                                    weight_rules[2][y][l] = 2*max_confid - 1;
                                    active_rules[2][y][l] = 1;
                                    //cout << max_confid << " " << l << " " << new_number_rules_mich_h << endl;
                                }
                                else
                                {
                                    active_rules[2][y][l] = 0;
                                    class_rules[2][y][l] = 0;
                                    confid_rules[2][y][l] = 0;
                                    weight_rules[2][y][l] = 0;
                                }
                                
                                delete[] obj_for_rule;
                                if (check == 1)
                                {
                                    break;
                                }
                            }
                        }
                    }
                    if (new_number_rules_mich_h <= 0)
                    {
                        break;
                    }
                }

                int num_ga_rule = 0;
                for (int l = 0; l < number_rules; l++)
                {
                    if (confid_rules[2][y][l] < better_than)
                    {
                        if (num_ga_rule > new_number_rules_mich_ga)
                        {
                            break;
                        }
                        else
                        {
                            for (int j = 0; j < columnNumber; j++)
                            {
                                pop3[y][l][j] = out[y][num_ga_rule][j];
                            }
                            for (int j = 0; j < num_class; j++)
                            {
                                in_confid[j] = 0;
                            }
                            confidence(num_class, lineNumber, columnNumber, class_answers, data, pop3[y][l], in_confid);
                            double max_confid = 0;
                            int c_confid = 0;
                            for (int j = 0; j < num_class; j++)
                            {
                                if (in_confid[j] > max_confid)
                                {
                                    max_confid = in_confid[j];
                                    c_confid = j;
                                }
                            }

                            if (max_confid > better_than)
                            {
                                class_rules[2][y][l] = c_confid;
                                confid_rules[2][y][l] = max_confid;
                                weight_rules[2][y][l] = 2*max_confid - 1;
                                active_rules[2][y][l] = 1;
                                
                                //cout << max_confid << " " << l << " " << num_ga_rule << endl;
                            }
                            else
                            {
                                active_rules[2][y][l] = 0;
                                class_rules[2][y][l] = 0;
                                confid_rules[2][y][l] = 0;
                                weight_rules[2][y][l] = 0;
                            }
                            num_ga_rule++;
                        }
                    }
                }
                //cout << endl;
                //cout << endl;
                //cout << endl;
            } 
            //cout << "---------------------------------" << endl;
            

            /*for (int y = 2; y < pop_size; y++)
            {
                cout << "Pop" << endl;
                for (int j = 0; j < number_rules; j++)
                {
                    cout << fitness_michegan[y][j] << endl;
                }
            }*/


//-------------------------------------------проверка----------------------------------------------------
            int best_index2 = 0;
            for (int y = 0; y < pop_size; y++)
            {
                //занулить конфиденс
                for (int l = 0; l < number_rules; l++)
                {
                    for (int j = 0; j < num_class; j++)
                    {
                        in_confid[j] = 0;
                    }
                    
                    confidence(num_class, lineNumber, columnNumber, class_answers, data, pop3[y][l], in_confid);
                    /*
                    for (int j = 0; j < num_class; j++)
                    {
                        cout << in_confid[j] << " ";
                    }*/

                    /*cout << endl << " Правило " << endl;
                    for (int j = 0; j < columnNumber; j++)
                    {
                        cout << in_rule[j] << " ";
                    }*/

                    double max_confid = 0;
                    int c_confid = 0;
                    for (int j = 0; j < num_class; j++)
                    {
                        if (in_confid[j] > max_confid)
                        {
                            max_confid = in_confid[j];
                            c_confid = j;
                        }
                    }

                    if (max_confid > better_than)
                    {
                        class_rules[2][y][l] = c_confid;
                        confid_rules[2][y][l] = max_confid;
                        weight_rules[2][y][l] = 2*max_confid - 1;
                        active_rules[2][y][l] = 1;
                    }
                    else
                    {
                        active_rules[2][y][l] = 0;
                        class_rules[2][y][l] = 0;
                        confid_rules[2][y][l] = 0;
                        weight_rules[2][y][l] = 0;
                    }
                }
                
                //Обнуление
                for (int j = 0; j < train_length; j++)
                {
                    reply_train[y][j] = 0;
                }

                for (int j = 0; j < train_length; j++)
                {
                    Rules(train_data[j], pop3[y], class_rules[2][y], confid_rules[2][y], columnNumber, number_rules, best_rule_for_object_train, reply_train, y, j);//отправка обучающей выборки
                }

                /*for (int j = 0; j < train_length; j++)
                {
                    cout << reply_train[y][j] << endl;
                }*/
                didntgetclass = 0;

                int train_error = 0;
                for (int l = 0; l < train_length; l++)
                {
                    if (reply_train[y][l] == -1)
                    {
                        didntgetclass++;
                    }
                    if (reply_train[y][l] != train_class_answers[l])
                    {
                        train_error = train_error + 1;//неправильно классифицированный объекты
                        correct_classification_for_object_train[y][l] = 0;//неправильно классифицированный объект
                    }
                    else
                    {
                        correct_classification_for_object_train[y][l] = 1;//правильно классифицированный объект
                    }
                }
                
                //Обнуление
                for (int j = 0; j < number_rules; j++)
                {
                    correct_classification_num[y][j] = 0;
                }

                //количество правильно классиф. объектов для каждого правила
                check_fitness_michegan(correct_classification_for_object_train[y], best_rule_for_object_train[y], y, number_rules, (lineNumber-(cross_num+last_data)), correct_classification_num); 
                
                for (int j = 0; j < number_rules; j++)
                {
                    if (confid_rules[2][y][j] < better_than)
                    {
                        active_rules[2][y][j] = 0;
                        class_rules[2][y][j] = 0;
                        confid_rules[2][y][j] = 0;
                        weight_rules[2][y][j] = 0;
                    }

                    if (correct_classification_num[y][j] == 0)
                    {
                        active_rules[2][y][j] = 0;
                        class_rules[2][y][j] = 0;
                        confid_rules[2][y][j] = 0;
                        weight_rules[2][y][j] = 0;
                    }
                }

                int flag_active = 0;
                flag_active = active_rule_flag(confid_rules[2][y], number_rules, better_than);

                double flag_not_dontcare = 0;
                flag_not_dontcare = dont_care_flag(number_rules, columnNumber, pop3[y], confid_rules[2][y], better_than);
                
                //cout << "train error " << train_error << " out of " << train_length << " with " << flag_active << endl;

                int** mfalse = new int* [num_class+1]; 
                for (int j = 0; j < num_class+1; j++)
                {
                    mfalse[j] = new int[num_class+1];
                }

                //Обнуление
                for (int j = 0; j < num_class+1; j++)
                {
                    for (int l = 0; l < num_class+1; l++)
                    {
                        mfalse[j][l] = 0;
                    }
                }

                //матрица ошибок для задач с N классами
                error_matrix(train_length, reply_train[y], train_class_answers, class_value, num_class, class_values_count_train, mfalse, didntgetclass);

                accuracy = 0;
                precision = 0;
                recall = 0;
                Fscore = 0;
                numerator = 0;
                denominator = 0; 
                b = 1;

                /*for (int j = 0; j < num_class+1; j++)
                {
                    for (int l = 0; l < num_class+1; l++)
                    {
                        cout << mfalse[j][l] << " ";
                    }
                    cout << endl;
                }*/
                //еще один столбик в матрице с неотнесенными объектами //ЕЩЕ МОМЕНТ

                for (int j = 0; j < num_class; j++)
                {
                    int sum_pre = 0;
                    int class_pre = 0;
                    int class_recall = 0;
                    int sum_recall = 0;
                    for (int l = 0; l < num_class; l++)
                    {
                        if (j == l)
                        {
                            numerator = numerator + mfalse[j][l];
                            class_pre = mfalse[j][l];
                            class_recall = mfalse[l][j];
                        }
                        denominator = denominator + mfalse[j][l];

                        sum_pre = sum_pre + mfalse[j][l];
                        sum_recall = sum_recall + mfalse[l][j];

                    }
                    if (sum_recall != 0)
                    {
                        recall = recall + (double)class_recall/(double)sum_recall;
                    }
                    if (sum_pre != 0)
                    {
                        precision = precision + (double)class_pre/(double)sum_pre;
                    }
                }

                accuracy = (double)numerator/(double)denominator;//для любого кол-ва классов
                precision = precision/(double)num_class;//для любого кол-ва классов
                recall = recall/(double)num_class;//для любого кол-ва классов
                if (precision + recall == 0)
                {
                    Fscore = 0;
                }
                else
                {
                    Fscore = (b * b + 1) * (precision*recall / (b * b* (precision + recall)));//для любого кол-ва классов
                }
                
               
                /*cout << endl << " Generation " << generation << endl;//Общая точность классификации
                cout << endl << " Accurancy " << accuracy << endl;//Общая точность классификации
                cout << endl << " Precision " << precision << endl;//Согласованность классификации первого класса с данными
                cout << endl << " Recall (Sensitivity) " << recall << endl;//Эффективность классификатора по выделению первого класса
                GitHub
                */

                for (int j = 0; j < num_class+1; j++)
                {
                    delete mfalse[j];
                }
                delete mfalse;

                error_percentage_train = double(train_error) / double(train_length);
                //f3 количество не донт care параметров в правиле суммарно по всем правилам 
                //реализовать NSGA-II
                fitness_small[y] = error_percentage_train;
                fitness[y] = w1*(1 - Fscore) + w2*flag_active + w3*flag_not_dontcare;//оптимизировать
                //cout << "Train percentage " << error_percentage_train << " Правил " << flag_active << " ДОНТ КЭР " << flag_not_dontcare <<  endl;
                //cout << endl << " Fitness " << fitness[y] << " y " << y;
               
                if (fitness[y] < best_fitness)
                {
                    best_accuracy = accuracy;
                    best_precision = precision;
                    best_recall = recall;
                    best_Fscore = Fscore;
                    num_rule_file = flag_active;

                    for (int l = 0; l < number_rules; l++)
                    {
                        for (int j = 0; j < columnNumber; j++)
                        {
                            best_rule_base[l][j] = 0;
                        }
                        best_class_rule_base[l] = 0;
                        best_active_rule_base[l] = 0;
                        best_confid_rule_base[l] = 0;
                    }
                    
                    best_fitness = fitness[y];
                    best_percentage = error_percentage_train;
                    
                    best_index = y;

                    for (int l = 0; l < number_rules; l++)
                    {
                        for (int j = 0; j < columnNumber; j++)
                        {
                            best_rule_base[l][j] = pop3[y][l][j];//база правил с наименьшей ошибкой
                        }
                        best_class_rule_base[l] = class_rules[2][y][l];
                        best_active_rule_base[l] = active_rules[2][y][l];
                        best_confid_rule_base[l] = confid_rules[2][y][l];
                    }
                    dont_care_file = dont_care_flag(number_rules, columnNumber, best_rule_base, best_confid_rule_base, better_than);

                    /*for (int l = 0; l < number_rules; l++)
                    {
                        for (int j = 0; j < columnNumber; j++)
                        {
                            cout << best_rule_base[l][j] << "\t";
                        }
                        cout << best_class_rule_base[l] << " " << best_active_rule_base[l] << " " << best_confid_rule_base[l] << endl;
                    }
                    cout << endl;*/

                }

            }
            //int test_flag_active = 0;
            //test_flag_active = active_rule_flag(best_confid_rule_base, number_rules, better_than);
            cout << i << "\t" << generation << "\t" << best_accuracy << "\t" << num_rule_file << endl;

            //cout << "test " << error_test << endl;
            //flag_active_best = active_rule_flag(confid_rules[2][best_child], number_rules);
            
            //cout << "Fitness train for all " << best_fitness << " Active rules " << flag_active_best;
            //cout << endl;

            //перезапись с лушими
            /*for (int l = 0; l < number_rules; l++)
            {
                for (int j = 0; j < columnNumber; j++)
                {
                    pop[0][l][j] = pop[best_index][l][j];
                }
                active_rules[0][0][l] = active_rules[0][best_index][l];
                class_rules[0][0][l] = class_rules[0][best_index][l];
                confid_rules[0][0][l] = confid_rules[0][best_index][l];
                weight_rules[0][0][l] = weight_rules[0][best_index][l];
            }*/

            for (int y = 0; y < pop_size; y++)
            {
                for (int l = 0; l < number_rules; l++)
                {
                    for (int j = 0; j < columnNumber; j++)
                    {
                        pop[y][l][j] = pop3[y][l][j];
                    }

                    active_rules[0][y][l] = active_rules[2][y][l];
                    class_rules[0][y][l] = class_rules[2][y][l];
                    confid_rules[0][y][l] = confid_rules[2][y][l];
                    weight_rules[0][y][l] = weight_rules[2][y][l];
                }
            }     

            if (generation > (gen - 2))
            {
                for (int j = 0; j < test_length; j++)
                {
                    Rules(test_data[j], best_rule_base, best_class_rule_base, best_confid_rule_base, columnNumber, number_rules, best_rule_for_object_test, reply_test, best_index, j);//отправка тестовой выборки
                }
                didntgetclasstest=0;
                int error_test = 0;
                for (int l = 0; l < test_length; l++)
                {
                    if (reply_test[best_index][l] == -1)
                    {
                        didntgetclasstest++;
                    }
                    if (reply_test[best_index][l] != test_class_answers[l])
                    {
                        error_test = error_test + 1;//пригодность
                    }
                }
                error_percentage_test = double(error_test) / ((double)cross_num_const+(double)last_data);
                
                int** mfalse = new int* [num_class+1]; 
                for (int j = 0; j < num_class+1; j++)
                {
                    mfalse[j] = new int[num_class+1];
                }

                //cout << "Error test % " << error_percentage_test;
                
                //Обнуление
                for (int j = 0; j < num_class+1; j++)
                {
                    for (int l = 0; l < num_class+1; l++)
                    {
                        mfalse[j][l] = 0;
                    }
                }

                //матрица ошибок для задач с N классами
                error_matrix(test_length, reply_test[best_index], test_class_answers, class_value, num_class, class_values_count_test, mfalse, didntgetclasstest);

                //cout << didntgetclasstest;

                accuracy_test = 0;
                precision_test = 0;
                recall_test = 0;
                Fscore_test = 0;
                numerator = 0;
                denominator = 0; 
                b = 1;

                /*for (int j = 0; j < num_class+1; j++)
                {
                    for (int l = 0; l < num_class+1; l++)
                    {
                        cout << mfalse[j][l] << " ";
                    }
                    cout << endl;
                }*/

                for (int j = 0; j < num_class; j++)
                {
                    int sum_pre = 0;
                    int class_pre = 0;
                    int class_recall = 0;
                    int sum_recall = 0;
                    for (int l = 0; l < num_class; l++)
                    {
                        if (j == l)
                        {
                            numerator = numerator + mfalse[j][l];
                            class_pre = mfalse[j][l];
                            class_recall = mfalse[l][j];
                        }
                        denominator = denominator + mfalse[j][l];

                        sum_pre = sum_pre + mfalse[j][l];
                        sum_recall = sum_recall + mfalse[l][j];

                    }
                    if (sum_recall != 0)
                    {
                        recall_test = recall_test + (double)class_recall/(double)sum_recall;
                    }
                    if (sum_pre != 0)
                    {
                        precision_test = precision_test + (double)class_pre/(double)sum_pre;
                    }
                }

                accuracy_test = (double)numerator/(double)denominator;//для любого кол-ва классов
                precision_test = precision_test/(double)num_class;//для любого кол-ва классов
                recall_test = recall_test/(double)num_class;//для любого кол-ва классов
                 if (precision_test + recall_test == 0)
                {
                    Fscore_test = 0;
                }
                else
                {
                    Fscore_test = (b * b + 1) * (precision_test*recall_test / (b * b* (precision_test + recall_test)));//для любого кол-ва классов
                }
                
                /*cout << endl << " Accurancy " << accuracy_test << endl;//Общая точность классификации
                cout << endl << " Precision " << precision_test << endl;//Согласованность классификации первого класса с данными
                cout << endl << " Recall (Sensitivity) " << recall_test << endl;//Эффективность классификатора по выделению первого класса
                cout << endl << " Fscore " << Fscore_test << endl;//Отношение между объектами первого класса в данных и предсказанными классификатором*/
                
                for (int j = 0; j < num_class+1; j++)
                {
                    for (int j1 = 0; j1 < num_class+1; j1++)
                    {
                        mas_error_classification[j][j1] = mfalse[j][j1];
                    }
                }

                for (int j = 0; j < num_class+1; j++)
                {
                    delete mfalse[j];
                }
                delete mfalse;
            }
            
//-------------------------------------------удаление массивов для generation----------------------------------------------------

        }
        for (int y = 0; y < pop_size; y++)
        {
            delete reply_test[y];
        }
        delete reply_test;
        
        for (int y = 0; y < pop_size; y++)
        {
            delete reply_train[y];
        }
        delete reply_train;

        delete[] rank;

        count_average_kfold[0][i] = best_accuracy;//точность на обучающей
        count_average_kfold[1][i] = precision; 
        count_average_kfold[2][i] = recall;
        count_average_kfold[3][i] = Fscore;
        count_average_kfold[4][i] = num_rule_file;//количество активных правил
        count_average_kfold[5][i] = dont_care_file;//длина правила 
        count_average_kfold[6][i] = accuracy_test;//точность на тестовой
        count_average_kfold[7][i] = precision_test;
        count_average_kfold[8][i] = recall_test;
        count_average_kfold[9][i] = Fscore_test;

        ofstream outpuut10;
        outpuut10.open(whatfileoutput.c_str(), ios::app);

        outpuut10 << best_accuracy << " " << best_precision << " " << best_recall << " " << best_Fscore << " " << num_rule_file << " " << dont_care_file << " " << accuracy_test << " " << precision_test << " " << recall_test << " " << Fscore_test << " " << endl; 

        outpuut10.close();

        ofstream outpuut1;

        string whatfileoutputmatrix;
        whatfileoutputmatrix = to_string(which_initial) + to_string(which_selection) + to_string(which_crossover) + to_string(which_mutation) + "matrix.txt";

        outpuut1.open(whatfileoutputmatrix.c_str(), ios::app);
        outpuut1 << i << endl;
        //запись матрицы в файл
        for (int j = 0; j < num_class+1; j++)
        {
            for (int j1 = 0; j1 < num_class+1; j1++)
            {
                outpuut1 << mas_error_classification[j][j1] << " ";
            }
            outpuut1 << endl;
        }
        outpuut1.close();

        ofstream outpuut2;

        string whatfileoutputrules;
        whatfileoutputrules = to_string(which_initial) + to_string(which_selection) + to_string(which_crossover) + to_string(which_mutation) + "rules.txt";

        outpuut2.open(whatfileoutputrules.c_str(), ios::app);
        outpuut2 << i << endl;

        //запись базы правил в файл
        for (int j = 0; j < number_rules; j++)
        {
            if (best_confid_rule_base[j] > better_than)
            {
                for (int l = 0; l < columnNumber; l++)
                {
                    outpuut2 << best_rule_base[j][l] << " ";
                }
                outpuut2 << best_class_rule_base[j] << "   " << best_active_rule_base[j] << "   " << best_confid_rule_base[j];
                outpuut2 << endl;
            }
        }
        outpuut2.close();
        
//-------------------------------------------удаление массивов для kfold----------------------------------------------------
        for (int ip = 0; ip < pop_size; ip++)
        {
            for (int j = 0; j < number_rules; j++)
            {
                delete[] pop[ip][j];
                delete[] pop2[ip][j];
                delete[] pop3[ip][j];
                delete[] out[ip][j];
            }
        }
        for (int ip = 0; ip < pop_size; ip++)
        {
            delete[] pop[ip];
            delete[] pop2[ip];
            delete[] pop3[ip];
            delete[] out[ip];
        }
        delete[] pop;
        delete[] pop2;
        delete[] pop3;
        delete[] out;

        for (int ip = 0; ip < pop_size; ip++)
        {
            delete best_rule_for_object_train[ip];
            delete best_rule_for_object_test[ip];
            delete correct_classification_for_object_train[ip];
            delete fitness_michegan[ip];
            delete correct_classification_num[ip];
        }
        delete best_rule_for_object_train;
        delete best_rule_for_object_test;
        delete correct_classification_for_object_train;
        delete fitness_michegan;
        delete correct_classification_num;

        delete[] class_values_count_train;
        delete[] class_values_count_test;
        delete[] in_rule;
        delete[] fitness;
        delete[] fitness_small;
        delete[] fitness_rang;
        delete[] fitness_rang_prop;

        for (int y = 0; y < number_rules; y++)
        {
            delete best_rule_base[y];
        }
        delete best_rule_base;
        delete[] best_class_rule_base;
        delete[] best_confid_rule_base;

        for (int ip = 0; ip < npop; ip++)
        {
            for (int j = 0; j < pop_size; j++)
            {
                delete[] confid_rules[ip][j];
                delete[] weight_rules[ip][j];
                delete[] active_rules[ip][j];
                delete[] class_rules[ip][j];
                delete[] rules_update[ip][j];
            }
        }
        for (int ip = 0; ip < npop; ip++)
        {
            delete[] confid_rules[ip];
            delete[] weight_rules[ip];
            delete[] active_rules[ip];
            delete[] class_rules[ip];
            delete[] rules_update[ip];
        }
        delete[] confid_rules;
        delete[] weight_rules;
        delete[] active_rules;
        delete[] class_rules;
        delete[] rules_update;

        for (int j = 0; j < num_class+1; j++)
        {
            delete mas_error_classification[j];
        }
        delete mas_error_classification;
    }

    for (int y = 0; y < cross_num_const+last_data; y++)
    {
        delete test_data[y];
    }
    delete test_data;

    for (int y = 0; y < lineNumber-(cross_num_const+last_data); y++)
    {
        delete train_data[y];
    }
    delete train_data;

    delete[] test_class_answers;
    delete[] train_class_answers;
    
    for (int i = 0; i < kfold; i++)
    {
        average_accuracy_kfold = average_accuracy_kfold + count_average_kfold[0][i];//точность на обучающей 0
        average_accuracy_test_kfold = average_accuracy_test_kfold + count_average_kfold[6][i];//точность на тестовой 6
        average_num_rule_file_kfold = average_num_rule_file_kfold + count_average_kfold[4][i];//количество активных правил 4
        average_dont_care_file_kfold = average_dont_care_file_kfold + count_average_kfold[5][i];//длина правила 5

        average_precision_kfold = average_precision_kfold + count_average_kfold[1][i];//1
        average_recall_kfold = average_recall_kfold + count_average_kfold[2][i];//2
        average_Fscore_kfold = average_Fscore_kfold + count_average_kfold[3][i];//3
    
        average_precision_test_kfold = average_precision_test_kfold + count_average_kfold[7][i];//7
        average_recall_test_kfold = average_recall_test_kfold + count_average_kfold[8][i];//8
        average_Fscore_test_kfold = average_Fscore_test_kfold + count_average_kfold[9][i];//9
    }

    average_accuracy_kfold = average_accuracy_kfold / double(kfold);//точность на обучающей 0
    average_accuracy_test_kfold = average_accuracy_test_kfold / double(kfold);//точность на тестовой 6
    average_num_rule_file_kfold = average_num_rule_file_kfold / double(kfold);//количество активных правил 4
    average_dont_care_file_kfold = average_dont_care_file_kfold / double(kfold);//длина правила 5

    average_precision_kfold = average_precision_kfold / double(kfold);//1
    average_recall_kfold = average_recall_kfold / double(kfold);//2
    average_Fscore_kfold = average_Fscore_kfold / double(kfold);//3

    average_precision_test_kfold = average_precision_test_kfold / double(kfold);//7
    average_recall_test_kfold = average_recall_test_kfold / double(kfold);//8
    average_Fscore_test_kfold = average_Fscore_test_kfold / double(kfold);//9

//-------------------------------------------вывод main в файл----------------------------------------------------
    
    ofstream outpuut;
    outpuut.open(whatfileoutput.c_str(), ios::app);

    //среднее по 10-fold cross-validation
    outpuut << average_accuracy_kfold << " " << average_precision_kfold << " " << average_recall_kfold << " " << average_Fscore_kfold << " " << average_num_rule_file_kfold << " " << average_dont_care_file_kfold << " " << average_accuracy_test_kfold << " " << average_precision_test_kfold << " " << average_recall_test_kfold << " " << average_Fscore_test_kfold << " " << endl; 

    outpuut.close();

                }
            }
        }
    }

//-------------------------------------------удаление массивов main----------------------------------------------------
    for (int y = 0; y < 10; y++)
    {
        delete count_average_kfold[y];
    }
    delete count_average_kfold;
    
    for (int i = 0; i < lineNumber; i++)
    {
        delete file_mas[i];
        delete data_file[i];
    }
    delete file_mas;
    delete data_file;

    delete[] class_answers;
    delete[] class_value;
    delete[] class_values_count;
    delete[] max_el;
    delete[] min_el;
    

    for (int i = 0; i < columnNumber; i++)
    {
        delete data[i];
    }
    delete data;

    return 0;
}
