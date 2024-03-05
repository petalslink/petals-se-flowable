/**
 * Copyright (c) 2018-2024 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable.admin;

import java.util.LinkedList;
import java.util.List;

import org.flowable.engine.ProcessEngine;
import org.flowable.engine.history.HistoricProcessInstance;
import org.flowable.engine.history.HistoricProcessInstanceQuery;
import org.flowable.engine.repository.ProcessDefinition;
import org.flowable.engine.runtime.ProcessInstanceQuery;
import org.flowable.task.api.history.HistoricTaskInstance;
import org.flowable.task.api.history.HistoricTaskInstanceQuery;
import org.ow2.petals.basisapi.exception.PetalsException;
import org.ow2.petals.flowable.FlowableSEConstants;
import org.ow2.petals.se.flowable.clientserver.api.admin.AdminRuntimeService;
import org.ow2.petals.se.flowable.clientserver.api.admin.exception.ProcessDefinitionNotFoundException;
import org.ow2.petals.se.flowable.clientserver.api.admin.exception.ProcessInstanceExistForDefinitionException;
import org.ow2.petals.se.flowable.clientserver.api.admin.exception.ProcessInstanceNotFoundException;

/**
 * Implementation of administrative operations.
 * 
 * @see AdminRuntimeService
 * @author Christophe DENEUX - Linagora
 *
 */
public final class AdminOperations {

    private AdminOperations() {
        // Utility class --> No constructor
    }

    /**
     * Admin utility operation associated to {@link AdminRuntimeService#listPurgeableProcessInstances(String, int)}
     */
    public static List<String> listPurgeableProcessInstances(final String processDefinitionKey,
            final int processDefinitionVersion, final ProcessEngine flowableEngine) throws PetalsException {
        final HistoricProcessInstanceQuery puregableProcInstQuery = flowableEngine.getHistoryService()
                .createHistoricProcessInstanceQuery();
        if (processDefinitionKey == null || processDefinitionKey.trim().isEmpty()) {
            // No filter on process definition. All process definitions taken into account
        } else {
            puregableProcInstQuery.processDefinitionKey(processDefinitionKey)
                    .processDefinitionVersion(processDefinitionVersion);
        }
        puregableProcInstQuery.finished();

        final List<HistoricProcessInstance> purgeableProcInsts = puregableProcInstQuery.list();
        final List<String> result = new LinkedList<>();
        for (final HistoricProcessInstance purgeableProcInst : purgeableProcInsts) {
            result.add(purgeableProcInst.getId());
        }
        return result;
    }

    /**
     * Admin utility operation associated to {@link AdminRuntimeService#purgeProcessInstance(String, boolean)}
     */
    public static List<String> purgeProcessInstance(final String procInstId, final boolean returnsCorrelatedFlows,
            final ProcessEngine flowableEngine) throws PetalsException {
        assert procInstId != null;
        assert !procInstId.trim().isEmpty();

        final List<String> result = new LinkedList<>();

        final HistoricProcessInstanceQuery procInstQuery = flowableEngine.getHistoryService()
                .createHistoricProcessInstanceQuery().processInstanceId(procInstId).includeProcessVariables();
        final HistoricProcessInstance procInst = procInstQuery.singleResult();
        if (procInst == null) {
            throw new ProcessInstanceNotFoundException(procInstId);
        } else {
            final String procInstFlowId = (String) procInst.getProcessVariables()
                    .get(FlowableSEConstants.Flowable.VAR_PETALS_FLOW_INSTANCE_ID);
            if (procInstFlowId != null) {
                result.add(procInstFlowId);
            }

            if (returnsCorrelatedFlows) {
                final String procInstCorrelatedFlowId = (String) procInst.getProcessVariables()
                        .get(FlowableSEConstants.Flowable.VAR_PETALS_CORRELATED_FLOW_INSTANCE_ID);
                if (procInstCorrelatedFlowId != null) {
                    result.add(procInstCorrelatedFlowId);
                }

                final HistoricTaskInstanceQuery procInstTasksQuery = flowableEngine.getHistoryService()
                        .createHistoricTaskInstanceQuery().processInstanceId(procInstId).includeTaskLocalVariables();
                // TODO: We should be able to filter on task local variable, but it seems to not work
                // .taskVariableExists(FlowableSEConstants.Flowable.VAR_PETALS_CORRELATED_FLOW_INSTANCE_ID);
                final List<HistoricTaskInstance> procInstTasks = procInstTasksQuery.list();
                for (final HistoricTaskInstance procInstTask : procInstTasks) {
                    final String procInstTaskCorrelatedFlowId = (String) procInstTask.getTaskLocalVariables()
                            .get(FlowableSEConstants.Flowable.VAR_PETALS_CORRELATED_FLOW_INSTANCE_ID);
                    if (procInstTaskCorrelatedFlowId != null) {
                        result.add(procInstTaskCorrelatedFlowId);
                    }
                }
            }

            flowableEngine.getHistoryService().deleteHistoricProcessInstance(procInstId);

            return result;
        }
    }

    /**
     * Admin utility operation associated to {@link AdminRuntimeService#undeployProcessDefinition(String, int)}
     */
    public static void undeployProcessDefinition(final String procDefKey, final int procDefVer,
            final ProcessEngine flowableEngine) throws PetalsException {

        // We look for the process definition
        final ProcessDefinition procDef = flowableEngine.getRepositoryService().createProcessDefinitionQuery()
                .processDefinitionKey(procDefKey).processDefinitionVersion(procDefVer).singleResult();
        if (procDef == null) {
            throw new ProcessDefinitionNotFoundException(procDefKey, procDefVer);
        }

        // Check if pending (active or suspended) process instances exist
        final ProcessInstanceQuery procInstsQuery = flowableEngine.getRuntimeService().createProcessInstanceQuery()
                .processDefinitionKey(procDefKey).processDefinitionVersion(procDefVer);
        if (procInstsQuery.count() > 0) {
            // Pending process instance exist for the given process definition. We can't undeploy it.
            throw new ProcessInstanceExistForDefinitionException(procDefKey, procDefVer);
        }

        // Check if ended process instances exist
        final HistoricProcessInstanceQuery endedProcInstsQuery = flowableEngine.getHistoryService()
                .createHistoricProcessInstanceQuery().processDefinitionKey(procDefKey)
                .processDefinitionVersion(procDefVer).finished();
        if (endedProcInstsQuery.count() > 0) {
            // Ended process instance exist for the given process definition. We can't undeploy it.
            throw new ProcessInstanceExistForDefinitionException(procDefKey, procDefVer);
        }

        // We undeploy the process definition
        try {
            flowableEngine.getRepositoryService().deleteDeployment(procDef.getDeploymentId());
        } catch (final RuntimeException e) {
            throw new PetalsException(
                    String.format("An error occurs trying to undeploy the process definition '%s' in its version %d",
                            procDefKey, procDefVer),
                    e);
        }

    }
}
