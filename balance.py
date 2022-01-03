"""结余断言生成脚本

这个脚本用来生成校园卡的 beancount 余额断言，用于进行 balance。

通过更改 `strftime('%Y%m%d')` 或 `strftime('%Y%m')` 可以分别达到
按天按月 balance 的效果。按天可以用于找出具体是那一天 unbalance。

在命令行下输入如下命令即可，其中 school_transaction.txt 是 tab 分隔
的校园卡流水。

```bash
python balance.py school_transaction.txt
```
"""

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
    return f'{date} balance Assets:CampusCard:Master {left:.2f} CNY'

print(df.groupby(df['交易时间'].dt.strftime('%Y%m')).apply(get_balance).to_list())
