Things done in data prep

# UNSW Claim data
1. change data type of vars.
2. remove data with "tenure < 0" and "total_claim_amount <= 0"
3. removing duplicated rows
4. remove rows with claim paid > 0 (but if claim status said "not_paid", 
still keep those rows with 0 claim paid)


# UNSW earned data
Q: what is lead_date_day? should i just remove it?
1. change data type of vars (mostly change chr vars into factor)
2. remove rows with 0 earned units (probably canceled within cooling off period)
3. remove rows with 0 tenure (why negative? operational thingy?) =>
effect of doing this => slightly decrease 158 rows total earned units (hence increase slightly claim_freq)
4.


# aggregated / combined data
- for earned data
1. find Total_Earned units for each exposure_id
2. collape using (group_by() and 

- for claim data
1. for each exposure id, find total claim_nb, claim_amount, claim_paid

- then combined the two data using exposure_id as key

How does earned units relate to nb_policy_inception_date