If you wish to read the full report, then refer to [report.pdf](./report.pdf)

The Orignal Dataset used in this project can be found on [kaggle](https://www.kaggle.com/datasets/tanayatipre/car-price-prediction-dataset) or [here](./latex-r-report-code/credit_eligibility.csv)

# Table of Content

- [Abstract](#abstract)
- [Univariate Analysis](#univariate-analysis)
- [Answering Non Trivial Questions](#answering-non-trivial-questions)

# Abstract

A crucial aspect in financial industry is assessing the likelihood of a customer repaying their debt. In an era where data-driven decision-making is highly important, understanding what factors have impact on creditworthiness is vital for both lenders and borrowers. This report analyzes a real-world dataset using data visualization and predictive modeling technique to examine relationships between factors like demographic characteristics, financial factors and customer characteristics and their effect on creditworthiness to answer [5 non-trivial research questions](#answering-non-trivial-questions).

# Univariate Analysis

![image](https://github.com/user-attachments/assets/9bf9f6ab-e695-4093-9b70-52214450430b)

The dataset containes data of significantly more male as compared with female customers.

![image](https://github.com/user-attachments/assets/00ac12c6-d808-41d6-992d-5cf441e22642)

There are total of five categories within the employment status of the customers. Half of the customers are employed and working. The customers with lowest count in the dataset are students.

![image](https://github.com/user-attachments/assets/33d0cfa0-30aa-4404-9dd3-69cd5b9137bf)

The vast majority of customers have pursued education up to at least the secondary level or higher. There are some customer who pursed high education but dropped out before completion.

![image](https://github.com/user-attachments/assets/3798a0ad-2aa7-47bf-8290-be6024f8100a)

More three-fifths of customers are married. The next largest group are those who are not married, followed by those who are separated or widowed.

![image](https://github.com/user-attachments/assets/3890a2d7-648a-48dd-bf3b-feca7ddcc2b1)

The majority of customers live in a home or apartment, and they make up more than 89% of the dataset. The rest of the dwellings (a small minority) consist of living with parents, municipal apartments, rented apartments, office apartments, and co-op apartments.

![image](https://github.com/user-attachments/assets/a6a236fd-4f1d-4e8b-9a0c-d9372cf0212d)

‘Laborers’, ‘Core Staff’ and ‘Sales Staff’ are the most common customer jobs, collectively representing over half of the dataset. ‘Managers’, ‘Drivers’, and ‘High Skill Tech Staff’ also contribute significantly to the job distribution.

![image](https://github.com/user-attachments/assets/b1e4756b-182e-48c3-81fa-971dbb4fe735)

More than three-fifths of the customer do not have cars. Customer with cars make up a smaller but still significant group within the dataset.

There are significantly more customers who owns property as compared with those who do not.

Surprisingly, most customers do not have neither a work phone nor a personal phone.

This could mean customer did not include their phone contact details during creation of their accounts.

Only around 9% of the customers provided their email address. Rest of the customers either did not provide it or do not have an email.

![image](https://github.com/user-attachments/assets/3a7e633e-b2f6-4849-a8d2-3ea3394cc3ea)

Almost all customers are classified as low risk in the dataset. High Risk customer makes up around 1.69% in the dataset.

![image](https://github.com/user-attachments/assets/a1a4ba50-a48a-4a14-98ab-457de6c4848f)

Most customers have no children. There is a single customer with highest number of children of 19. Majority of the customers have either no children, 1 child or 2 children

![image](https://github.com/user-attachments/assets/09fd06ee-7ef7-41fd-8240-298f627966a8)

The majority of customers report having 2 family members, followed by those reporting 1 or 3 family members with nearly equal proportions between the two categories.

![image](https://github.com/user-attachments/assets/7ccabcde-489a-4056-a00e-31cdc90cc0b1)

When comparing the ages of customers, a higher proportion falls into the younger age group.

![image](https://github.com/user-attachments/assets/05a5679b-b0ac-4974-b33f-80124fedb448)

Similar trend is observed in the distribution of relative age of accounts where comparatively more proportion of accounts seem to created recently.

![image](https://github.com/user-attachments/assets/02e4bc56-475a-421d-9fe3-dcfea1e55f9d)

In the ‘Employment.relative.length’ column, negative values mean employment periods. Significantly higher proportion of customers are employed.

![image](https://github.com/user-attachments/assets/19d717af-0577-4641-a98a-48dc9c7cf87a)

<br>

# Answering Non Trivial Questions

1. What factors greatly contribute to risk status of customers for credit card?

    ![image](https://github.com/user-attachments/assets/d029a83f-6229-466c-a51a-138704d6f16e)

    ![image](https://github.com/user-attachments/assets/1648a6bb-b183-45ec-a531-da8f4290faca)

    Employment tenure and customer age demonstrated the strongest impact on risk status, followed by account age. How much a customer earn, and to a lesser extent how many family members they have, also contributes. This suggests that stability and longevity in personal and financial aspects are more predictive of credit card risk. Given all these points, financial stability and longevity in personal and financial aspects are key predictors of credit card risk, with employment length and customer age exhibiting the most pronounced influence when classifying customers into high risk or low risk.

<br>

2. Which professions have a higher chance of being classified as high risk?

    ![image](https://github.com/user-attachments/assets/6fd97ca0-1b95-4f70-bc9a-a5360c63d6f9)

    ![image](https://github.com/user-attachments/assets/03c4d9ba-dab5-4332-a529-84480583eb77)

    ‘It Staff’ and ‘Low-Skill Laborers’ job are at risk the most as compared with others. On the other hand, ‘Cleaning Staff’, ‘Medicine Staff’, ‘Private Service Staff’ and ‘Realty Agents’ jobs have low risk associated, with ‘Realty Agents’ job being practically risk free. Moderate risk jobs has a wider range of incomes than the other two risk classes.

    It includes both high-income earners and those with lower incomes. In light of these information, ‘IT Staff’ (4.6%) and ‘Low-Skill Laborers’ (5%) includes a disproportionately high percentage of high-risk individuals, significantly higher than the average proportion of 1.7% across moderate risk job titles and less than 1% across low risk job titles.

    Upon investigating their income pattern, it can be observed that jobs with highly skewed income distribution were high risk. Moderate risk jobs follow a similar normal distribution, while low risk jobs often have a more concentrated income range.

<br>

3. How does employment duration and status influence income levels among customers?

    ![image](https://github.com/user-attachments/assets/1c47ce60-ee1f-48ed-9970-f28234ab2781)

    The findings suggests that employment status and duration are indeed important contributors to observed earnings differentials. The majority of high-income individuals (defined as those earning above the 90th percentile) are concentrated in the ‘Commercial Associate’ category. Other high income customers have employment status either ‘State Servant’ or ‘Working’.

    Furthermore, early years in career seem pretty key to broader earning opportunity later on, especially for those in ‘Commercial Associate’ roles. For those in ‘Working’ status, employment duration is associated with income in a non-linear way with income increasing at first but showing a potential plateau or decline after a certain point.

<br>

4. Does asset ownership significantly impact income levels, and does this impact differ between high-risk and low-risk customers?

    ![image](https://github.com/user-attachments/assets/b5b9ca7b-27d5-4af2-a016-dc94814504e1)

    The results show a positive association between asset ownership and income. Customers who own property tend to have higher incomes than those without. In the same way car ownership indicates a higher income. These findings suggest that asset accumulation plays a role in influencing earning potential.

    The study also identified a unique pattern for high-risk customers: higher and a highly skewed distribution of income. This means that even though people that are high-risk tend to earn more in total, their income levels are also more spread out with a good number of them having very high incomes.

    To sum up, the results show a positive association between asset ownership and in- come, and high-risk customers exhibited a higher and more skewed income distribution, highlighting the need for targeted financial products that address income variability.

<br>

5. Does higher education level mean high income?

    ![image](https://github.com/user-attachments/assets/ab812626-a847-495a-85e7-61cb67a5eb92)

    We see a positive correlation between education level and median income.It shows a strong upwards trend with an increase in median income with every higher level of educational attainment, from Lower Secondary to Academic Degree. Those having an Academic Degree make nearly the double the median income of those with Lower Secondary education. In other words, in the simplest of terms, it illustrates even more clearly that a higher educational attainment leads to a higher ability to earn, and supports the notion that education plays a crucial role in socioeconomic advancement.

