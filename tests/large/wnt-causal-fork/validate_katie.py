#! /usr/bin/env python3

import argparse
import pandas as pd
import sys
from pathlib import Path
from typing import Dict, Union
from KaSaAn.core import KappaSnapshot
from KaSaAn.functions import find_snapshot_names


def main():
    """Script to validate the KaTIE outputs against KaSim-produced snapshots. Returns a pandas DataFrame holding the
     result of the comparison. Successful validation means the DataFrame is empty. The DataFrame's columns are the
     names assigned in the KaTIE query to the measures being taken (column names in the resulting CSV); row labels are
     the agent identifier for the Cat agent in that story."""

    sim_out_dir = Path('kasim-output')

    global_tracker: Dict[int, Dict[str, Union[int, float]]] = {}
    """Structure to hold information; beginning with the query's root, then using the time-stamps to constrain
     the search of the next pattern, repeatedly."""

    def valid_delta_funct(
            pre_snap_name: str,
            this_event_label: str,
            next_event_time_label: str) -> Union[Dict[str, Union[int, float]], None]:
        """If snapshot holds a valid delta, return a dictionnary of values."""

        pre_snap = KappaSnapshot(pre_snap_name)
        series_name = pre_snap_name.split('_')[1]
        port_name = series_name[:-1]
        pst_snap_name = Path(sim_out_dir / 'snap_{}_pst_{}.ka'.format(series_name, pre_snap.get_snapshot_event() + 1))
        pst_snap = KappaSnapshot(pst_snap_name)
        for this_complex in pre_snap.get_all_complexes():
            for this_agent in this_complex.get_all_agents():
                if this_agent.get_agent_name() == 'Cat':
                    ix = this_agent.get_agent_identifier()
                    if ix is None:
                        raise ValueError(
                            '{} has agent {} lacking identifier!'.format(pre_snap.get_snapshot_file_name(), this_agent))
                    else:
                        that_agent = pst_snap.get_agent_from_identifier(ix)
                        if that_agent is None:
                            # we found a degradation event: root!
                            port_of_interest = this_agent.get_port('S5')
                            if port_of_interest is None:
                                raise ValueError('Agent {} does not have expected site {}!'.format(
                                    this_agent, port_name))
                            else:
                                bond_id = port_of_interest.get_port_bond_state()
                            complex_of_interest = pre_snap.get_complex_of_agent(ix)
                            if complex_of_interest is None:
                                raise ValueError(
                                    '{} did no complex holding agent {}! Snapshot parse error'.format(
                                        pre_snap.get_snapshot_file_name(), ix))
                            else:
                                bond = complex_of_interest.get_agents_of_bond(bond_id)
                                if bond is None:
                                    raise ValueError('Snapshot parse error! bond {}'.format(bond_id))
                                else:
                                    [b_1, b_2] = bond
                            if b_1 == this_agent:
                                partner = b_2
                            else:
                                partner = b_1
                            other_id = partner.get_agent_identifier()
                            if other_id is None:
                                raise ValueError('Agent {} should have an id!'.format(partner))
                            return {
                                'id_cat': ix,
                                'deg_u_T': pre_snap.get_snapshot_time(),
                                'deg_u_E': pst_snap.get_snapshot_event(),
                                'Axin_deg_u': complex_of_interest.get_number_of_embeddings_of_agent('Axn()'),
                                'APC_deg_u': complex_of_interest.get_number_of_embeddings_of_agent('APC()'),
                                'size_deg_u': complex_of_interest.get_size_of_complex(),
                                'id_prot_deg_u': other_id
                                }
                        elif that_agent.get_agent_name() != 'Cat':
                            raise ValueError(
                                'Agent changed identity! {} -> {} in\n{} -> {}'.format(
                                    this_agent, that_agent,
                                    pre_snap.get_snapshot_file_name(), pst_snap.get_snapshot_file_name()))
                        else:
                            if this_agent.get_port(port_name) == that_agent.get_port(port_name):
                                pass
                            else:
                                if ix not in global_tracker.keys():
                                    # degradation not observed for this agent
                                    pass
                                elif this_event_label in global_tracker[ix]:
                                    # already found a valid entry
                                    pass
                                elif pre_snap.get_snapshot_time() > global_tracker[ix][next_event_time_label]:
                                    # this event occurs after the next event; victim of a do-undo loop
                                    pass
                                else:
                                    # if we got here, all sanity checks passed
                                    port_of_interest = this_agent.get_port(port_name)
                                    if port_of_interest is None:
                                        raise ValueError('Agent {} does not have expected site {}!'.format(
                                            this_agent, port_name))
                                    else:
                                        bond_id = port_of_interest.get_port_bond_state()
                                    complex_of_interest = pre_snap.get_complex_of_agent(ix)
                                    if complex_of_interest is None:
                                        raise ValueError(
                                            '{} did no complex holding agent {}! Snapshot parse error'.format(
                                                pre_snap.get_snapshot_file_name(), ix))
                                    else:
                                        bond = complex_of_interest.get_agents_of_bond(bond_id)
                                        if bond is None:
                                            raise ValueError('Snapshot parse error! bond {}'.format(bond_id))
                                        else:
                                            [b_1, b_2] = bond
                                    if b_1 == this_agent:
                                        partner = b_2
                                    else:
                                        partner = b_1
                                    other_id = partner.get_agent_identifier()
                                    if other_id is None:
                                        raise ValueError('Agent {} should have an id!'.format(partner))
                                    else:
                                        return {
                                            'ix': ix,                                                               # Identifier of Cat agent
                                            'time': pre_snap.get_snapshot_time(),                                   # Value of simulation time when the event fired (before inter-event time advancement)
                                            'event': pst_snap.get_snapshot_event(),                                 # Value of event counter after this event has finished
                                            'axn': complex_of_interest.get_number_of_embeddings_of_agent('Axn()'),  # Abundance of Axn-type agent in complex containing the Cat matched.
                                            'apc': complex_of_interest.get_number_of_embeddings_of_agent('APC()'),  # Abundance of APC-type agent in complex containing the Cat matched.
                                            'size': complex_of_interest.get_size_of_complex(),                      # Number of agents in complex containing the Cat matched.
                                            'partner_id': other_id                                                  # Identifier of the partner modifying the matched Cat
                                            }
        return None

    # deal with: degradation
    # This search is done in time-forward, to keep the order of stories as reported by KaTie
    pre_snap_names = find_snapshot_names(sim_out_dir, 'snap_Sfs_pre_*.ka')
    for pre_snap_name in pre_snap_names:
        res_dict = valid_delta_funct(pre_snap_name, '', '')
        if res_dict is None:
            raise ValueError('Error processing snap {}'.format(pre_snap_name))
        else:
            ix = res_dict['id_cat']
            global_tracker[ix] = {
                'deg_u_T': res_dict['deg_u_T'],
                'deg_u_E': res_dict['deg_u_E'],
                'Axin_deg_u': res_dict['Axin_deg_u'],
                'APC_deg_u': res_dict['APC_deg_u'],
                'size_deg_u': res_dict['size_deg_u'],
                'id_prot_deg_u': res_dict['id_prot_deg_u']
            }

    # deal with: ubi_u & S5
    # This and following searches are done in time-reverse, as the degradation roots the stories
    pre_snap_names = find_snapshot_names(sim_out_dir, 'snap_S5s_pre_*.ka')
    pre_snap_names.reverse()
    ix_to_check = set(global_tracker.keys())
    while len(ix_to_check) > 0:
        for pre_snap_name in pre_snap_names:
            res_dict = valid_delta_funct(pre_snap_name, 'ubi_u_T', 'deg_u_T')
            if res_dict is None:
                pass
            else:
                ix = res_dict['ix']
                global_tracker[ix]['ubi_u_T'] = res_dict['time']
                global_tracker[ix]['ubi_u_E'] = res_dict['event']
                global_tracker[ix]['Axin_ubi_u'] = res_dict['axn']
                global_tracker[ix]['APC_ubi_u'] = res_dict['apc']
                global_tracker[ix]['size_ubi_u'] = res_dict['size']
                global_tracker[ix]['id_trcp_ubi_u'] = res_dict['partner_id']
                ix_to_check.remove(ix)

    # deal with: S33_u & S4
    pre_snap_names = find_snapshot_names(sim_out_dir, 'snap_S4s_pre_*.ka')
    pre_snap_names.reverse()
    ix_to_check = set(global_tracker.keys())
    while len(ix_to_check) > 0:
        for pre_snap_name in pre_snap_names:
            res_dict = valid_delta_funct(pre_snap_name, 'S33_u_T', 'ubi_u_T')
            if res_dict is None:
                pass
            else:
                ix = res_dict['ix']
                global_tracker[ix]['S33_u_T'] = res_dict['time']
                global_tracker[ix]['S33_u_E'] = res_dict['event']
                global_tracker[ix]['Axin_S33_u'] = res_dict['axn']
                global_tracker[ix]['APC_S33_u'] = res_dict['apc']
                global_tracker[ix]['size_S33_u'] = res_dict['size']
                global_tracker[ix]['id_gsk3_S33_u'] = res_dict['partner_id']
                ix_to_check.remove(ix)

    # deal with: S37_u & S3
    pre_snap_names = find_snapshot_names(sim_out_dir, 'snap_S3s_pre_*.ka')
    pre_snap_names.reverse()
    ix_to_check = set(global_tracker.keys())
    while len(ix_to_check) > 0:
        for pre_snap_name in pre_snap_names:
            res_dict = valid_delta_funct(pre_snap_name, 'S37_u_T', 'ubi_u_T')
            if res_dict is None:
                pass
            else:
                ix = res_dict['ix']
                global_tracker[ix]['S37_u_T'] = res_dict['time']
                global_tracker[ix]['S37_u_E'] = res_dict['event']
                global_tracker[ix]['Axin_S37_u'] = res_dict['axn']
                global_tracker[ix]['APC_S37_u'] = res_dict['apc']
                global_tracker[ix]['size_S37_u'] = res_dict['size']
                global_tracker[ix]['id_gsk3_S37_u'] = res_dict['partner_id']
                ix_to_check.remove(ix)

    # deal with: T41_a & S2
    pre_snap_names = find_snapshot_names(sim_out_dir, 'snap_S2s_pre_*.ka')
    pre_snap_names.reverse()
    ix_to_check = set(global_tracker.keys())
    while len(ix_to_check) > 0:
        for pre_snap_name in pre_snap_names:
            res_dict = valid_delta_funct(pre_snap_name, 'T41_a_T', 'S37_u_T')
            if res_dict is None:
                pass
            else:
                ix = res_dict['ix']
                global_tracker[ix]['T41_a_T'] = res_dict['time']
                global_tracker[ix]['T41_a_E'] = res_dict['event']
                global_tracker[ix]['Axin_T41_a'] = res_dict['axn']
                global_tracker[ix]['APC_T41_a'] = res_dict['apc']
                global_tracker[ix]['size_T41_a'] = res_dict['size']
                global_tracker[ix]['id_gsk3_T41_a'] = res_dict['partner_id']
                ix_to_check.remove(ix)

    # deal with: T41_b & S2
    pre_snap_names = find_snapshot_names(sim_out_dir, 'snap_S2s_pre_*.ka')
    pre_snap_names.reverse()
    ix_to_check = set(global_tracker.keys())
    while len(ix_to_check) > 0:
        for pre_snap_name in pre_snap_names:
            res_dict = valid_delta_funct(pre_snap_name, 'T41_b_T', 'S33_u_T')
            if res_dict is None:
                pass
            else:
                ix = res_dict['ix']
                global_tracker[ix]['T41_b_T'] = res_dict['time']
                global_tracker[ix]['T41_b_E'] = res_dict['event']
                global_tracker[ix]['Axin_T41_b'] = res_dict['axn']
                global_tracker[ix]['APC_T41_b'] = res_dict['apc']
                global_tracker[ix]['size_T41_b'] = res_dict['size']
                global_tracker[ix]['id_gsk3_T41_b'] = res_dict['partner_id']
                ix_to_check.remove(ix)

    # deal with: S45_a & S1
    pre_snap_names = find_snapshot_names(sim_out_dir, 'snap_S1s_pre_*.ka')
    pre_snap_names.reverse()
    ix_to_check = set(global_tracker.keys())
    while len(ix_to_check) > 0:
        for pre_snap_name in pre_snap_names:
            res_dict = valid_delta_funct(pre_snap_name, 'S45_a_T', 'T41_a_T')
            if res_dict is None:
                pass
            else:
                ix = res_dict['ix']
                global_tracker[ix]['S45_a_T'] = res_dict['time']
                global_tracker[ix]['S45_a_E'] = res_dict['event']
                global_tracker[ix]['Axin_S45_a'] = res_dict['axn']
                global_tracker[ix]['APC_S45_a'] = res_dict['apc']
                global_tracker[ix]['size_S45_a'] = res_dict['size']
                global_tracker[ix]['id_ck1a_S45_a'] = res_dict['partner_id']
                ix_to_check.remove(ix)

    # deal with: S45_b & S1
    pre_snap_names = find_snapshot_names(sim_out_dir, 'snap_S1s_pre_*.ka')
    pre_snap_names.reverse()
    ix_to_check = set(global_tracker.keys())
    while len(ix_to_check) > 0:
        for pre_snap_name in pre_snap_names:
            res_dict = valid_delta_funct(pre_snap_name, 'S45_b_T', 'T41_b_T')
            if res_dict is None:
                pass
            else:
                ix = res_dict['ix']
                global_tracker[ix]['S45_b_T'] = res_dict['time']
                global_tracker[ix]['S45_b_E'] = res_dict['event']
                global_tracker[ix]['Axin_S45_b'] = res_dict['axn']
                global_tracker[ix]['APC_S45_b'] = res_dict['apc']
                global_tracker[ix]['size_S45_b'] = res_dict['size']
                global_tracker[ix]['id_ck1a_S45_b'] = res_dict['partner_id']
                ix_to_check.remove(ix)

    # deal with: the origin of stories
    creation_snap_pre = KappaSnapshot(sim_out_dir / 'snap_creation_pre.ka')
    creation_snap_pst = KappaSnapshot(sim_out_dir / 'snap_creation_pst.ka')
    for ix in global_tracker.keys():
        agent_not_exists_pre = creation_snap_pre.get_agent_from_identifier(ix) is None
        agent_exists_pst = creation_snap_pst.get_agent_from_identifier(ix) is not None
        if agent_not_exists_pre and agent_exists_pst:
            global_tracker[ix]['cre_u_T'] = creation_snap_pre.get_snapshot_time()
            global_tracker[ix]['cre_u_E'] = creation_snap_pre.get_snapshot_event() + 1
            # note pre & pst have same KaSim event number;
            # KaSim did not count the introduction step as an event!

    # validation
    katie_data = pd.read_csv(
        'katie-output/results/query_out_kinase_complex_characterization.csv',
        skipinitialspace=True, quotechar="'", index_col='id_cat')

    kasim_data = pd.DataFrame.from_dict(data=global_tracker, orient='index')

    # KaSim kappa snapshots only print 4 decimals, so rounding is necessary
    diff_data_frame = kasim_data.compare(katie_data.round(4), result_names=('KaSim', 'KaTIE'))
    return diff_data_frame


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=main.__doc__)
    parser.parse_args()

    result_data_frame = main()
    if any(result_data_frame):
        raise ValueError(
            'Data does not match! Rows indexed by agent identifier, columns by measure label; diff is\n{}'.format(
                result_data_frame))
    else:
        sys.exit(0)
