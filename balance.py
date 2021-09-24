import sys
import datetime
import pandas as pd


file_path = sys.argv[1]
df = pd.read_csv(file_path, sep='\t', names=['交易时间', '商户名称', '交易名称', '交易金额', '卡余额'], skiprows=1)

df['交易时间'] = pd.to_datetime(df['交易时间'])
df['商户名称'] = df['商户名称'].str.strip()
df['交易名称'] = df['交易名称'].str.strip()

def get_balance(group):
    date = group['交易时间'].iloc[0].to_pydatetime().date() + datetime.timedelta(days=1)
    left = group['卡余额'].iloc[0]
    return f'{date} balance Assets:CampusCard:Master {left} CNY'

print(df.groupby(df['交易时间'].dt.strftime('%Y%m')).apply(get_balance).to_list())
