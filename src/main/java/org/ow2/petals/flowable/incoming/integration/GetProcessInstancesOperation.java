/**
 * Copyright (c) 2015-2025 Linagora
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
package org.ow2.petals.flowable.incoming.integration;

import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_GETPROCESSINSTANCES;

import java.net.URI;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;

import org.flowable.engine.HistoryService;
import org.flowable.engine.RepositoryService;
import org.flowable.engine.RuntimeService;
import org.flowable.engine.history.HistoricProcessInstance;
import org.flowable.engine.history.HistoricProcessInstanceQuery;
import org.flowable.engine.repository.ProcessDefinition;
import org.flowable.engine.repository.ProcessDefinitionQuery;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.runtime.ProcessInstanceQuery;
import org.ow2.petals.components.flowable.generic._1.GetProcessInstances;
import org.ow2.petals.components.flowable.generic._1.GetProcessInstancesResponse;
import org.ow2.petals.components.flowable.generic._1.InvalidRequest;
import org.ow2.petals.components.flowable.generic._1.ProcessInstanceState;
import org.ow2.petals.components.flowable.generic._1.ProcessInstances;
import org.ow2.petals.components.flowable.generic._1.Variable;
import org.ow2.petals.components.flowable.generic._1.Variables;
import org.ow2.petals.flowable.incoming.integration.exception.OperationInitializationException;

/**
 * The integration operation to search process instances
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class GetProcessInstancesOperation extends AbstractOperation<GetProcessInstances, GetProcessInstancesResponse> {

    public static final URI FAULT_ACTOR = URI
            .create("http://petals.ow2.org/components/flowable/generic/1.0/GetProcessInstances");

    /**
     * The Flowable task service
     */
    private final RuntimeService runtimeService;

    /**
     * The Flowable history service
     */
    private final HistoryService historyService;

    /**
     * The Flowable repository service
     */
    private final RepositoryService repositoryService;

    public GetProcessInstancesOperation(final RuntimeService runtimeService, final HistoryService historyService,
            final RepositoryService repositoryService, final Logger log) throws OperationInitializationException {
        super(ITG_OP_GETPROCESSINSTANCES, FAULT_ACTOR, new Class[] { GetProcessInstances.class,
                GetProcessInstancesResponse.class, InvalidRequest.class }, log);
        this.runtimeService = runtimeService;
        this.historyService = historyService;
        this.repositoryService = repositoryService;
    }

    @Override
    public GetProcessInstancesResponse doExecute(final GetProcessInstances incomingObject) throws Exception {

        final ProcessInstanceState state = incomingObject.getState();
        if (state == null) {
            // By default, we search all process instances independently of their state
            final GetProcessInstancesResponse result = this.searchProcessInstances(incomingObject, null);
            final GetProcessInstancesResponse resultFinished = this.searchHistoricProcessInstances(incomingObject,
                    ProcessInstanceState.FINISHED);

            this.merge(resultFinished, result);

            return result;
        } else if (state == ProcessInstanceState.ACTIVE) {
            return this.searchProcessInstances(incomingObject, ProcessInstanceState.ACTIVE);
        } else if (state == ProcessInstanceState.SUSPENDED) {
            return this.searchProcessInstances(incomingObject, ProcessInstanceState.SUSPENDED);
        } else {
            assert state == ProcessInstanceState.FINISHED;
            return this.searchHistoricProcessInstances(incomingObject, ProcessInstanceState.FINISHED);
        }
    }

    private void merge(final GetProcessInstancesResponse from, final GetProcessInstancesResponse into) {

        for (final org.ow2.petals.components.flowable.generic._1.ProcessInstance procInstToMerge : from
                .getProcessInstances().getProcessInstance()) {
            boolean procInstKnown = false;
            for (final org.ow2.petals.components.flowable.generic._1.ProcessInstance procInst : into
                    .getProcessInstances().getProcessInstance()) {
                if (procInstToMerge.getProcessInstanceIdentifier().equals(procInst.getProcessInstanceIdentifier())) {
                    procInstKnown = true;
                    break;
                }
            }
            if (!procInstKnown) {
                into.getProcessInstances().getProcessInstance().add(procInstToMerge);
            }
        }
    }

    /**
     * Search process instances that are not ended (in state 'active' or 'suspended').
     */
    private GetProcessInstancesResponse searchProcessInstances(final GetProcessInstances incomingObject,
            final ProcessInstanceState state) throws MessagingException {

        final ProcessInstanceQuery processInstanceQuery = this.runtimeService.createProcessInstanceQuery();

        if (state == ProcessInstanceState.ACTIVE) {
            processInstanceQuery.active();
        } else if (state == ProcessInstanceState.SUSPENDED) {
            processInstanceQuery.suspended();
        } else {
            // we search all process instances independently of their state: ACTIVE and SUSPENDED
            assert state == null;
        }

        final String processDefinitionId = incomingObject.getProcessDefinitionIdentifier();
        if (processDefinitionId != null && !processDefinitionId.isEmpty()) {
            processInstanceQuery.processDefinitionKey(processDefinitionId);
        }

        final String processInstanceId = incomingObject.getProcessInstanceIdentifier();
        if (processInstanceId != null && !processInstanceId.isEmpty()) {
            processInstanceQuery.processInstanceId(processInstanceId);
        }

        final Variables variables = incomingObject.getVariables();
        if (variables != null && !variables.getVariable().isEmpty()) {
            for (final Variable variable : variables.getVariable()) {
                processInstanceQuery.variableValueEquals(variable.getName(), Utils.parseVariableValue(variable));
            }
        }

        processInstanceQuery.includeProcessVariables();

        final GetProcessInstancesResponse response = new GetProcessInstancesResponse();
        final ProcessInstances responseProcessInstances = new ProcessInstances();
        response.setProcessInstances(responseProcessInstances);
        final List<ProcessInstance> processInstances = processInstanceQuery.list();
        for (final ProcessInstance processInstance : processInstances) {
            final org.ow2.petals.components.flowable.generic._1.ProcessInstance responseProcessInstance = new org.ow2.petals.components.flowable.generic._1.ProcessInstance();
            responseProcessInstances.getProcessInstance().add(responseProcessInstance);

            responseProcessInstance.setProcessInstanceIdentifier(processInstance.getProcessInstanceId());

            if (processInstance.isSuspended()) {
                responseProcessInstance.setState(ProcessInstanceState.SUSPENDED);
            } else if (processInstance.isEnded()) {
                responseProcessInstance.setState(ProcessInstanceState.FINISHED);
            } else {
                responseProcessInstance.setState(ProcessInstanceState.ACTIVE);
            }

            final ProcessDefinitionQuery processDefQuery = this.repositoryService.createProcessDefinitionQuery()
                    .processDefinitionId(processInstance.getProcessDefinitionId());
            final ProcessDefinition processDefinition = processDefQuery.singleResult();
            responseProcessInstance.setProcessDefinitionIdentifier(processDefinition.getKey());

            responseProcessInstance.setVariables(Utils.buildVariables(processInstance.getProcessVariables(), this.log));
        }

        return response;
    }

    /**
     * Search process instances that are ended (in state 'finished').
     */
    private GetProcessInstancesResponse searchHistoricProcessInstances(final GetProcessInstances incomingObject,
            final ProcessInstanceState state) {

        final HistoricProcessInstanceQuery processInstanceQuery = this.historyService
                .createHistoricProcessInstanceQuery();

        processInstanceQuery.finished();

        final String processDefinitionId = incomingObject.getProcessDefinitionIdentifier();
        if (processDefinitionId != null && !processDefinitionId.isEmpty()) {
            processInstanceQuery.processDefinitionKey(processDefinitionId);
        }

        final String processInstanceId = incomingObject.getProcessInstanceIdentifier();
        if (processInstanceId != null && !processInstanceId.isEmpty()) {
            processInstanceQuery.processInstanceId(processInstanceId);
        }

        final Variables variables = incomingObject.getVariables();
        if (variables != null && !variables.getVariable().isEmpty()) {
            for (final Variable variable : variables.getVariable()) {
                processInstanceQuery.variableValueEquals(variable.getName(), variable.getValue());
            }
        }

        processInstanceQuery.includeProcessVariables();

        final GetProcessInstancesResponse response = new GetProcessInstancesResponse();
        final ProcessInstances responseProcessInstances = new ProcessInstances();
        response.setProcessInstances(responseProcessInstances);
        final List<HistoricProcessInstance> processInstances = processInstanceQuery.list();
        for (final HistoricProcessInstance processInstance : processInstances) {
            final org.ow2.petals.components.flowable.generic._1.ProcessInstance responseProcessInstance = new org.ow2.petals.components.flowable.generic._1.ProcessInstance();
            responseProcessInstances.getProcessInstance().add(responseProcessInstance);

            responseProcessInstance.setProcessInstanceIdentifier(processInstance.getId());
            responseProcessInstance.setState(ProcessInstanceState.FINISHED);

            final ProcessDefinitionQuery processDefQuery = this.repositoryService.createProcessDefinitionQuery()
                    .processDefinitionId(processInstance.getProcessDefinitionId());
            final ProcessDefinition processDefinition = processDefQuery.singleResult();
            responseProcessInstance.setProcessDefinitionIdentifier(processDefinition.getKey());

            final Variables responseProcessVariables = new Variables();
            responseProcessInstance.setVariables(responseProcessVariables);
            for (final Entry<String, Object> variable : processInstance.getProcessVariables().entrySet()) {

                final Variable responseProcessVariable = new Variable();
                final Object variableValue = variable.getValue();
                responseProcessVariable.setName(variable.getKey());
                if (variableValue instanceof Date) {
                    final GregorianCalendar calendar = new GregorianCalendar();
                    calendar.setTime((Date) variableValue);
                    try {
                        responseProcessVariable.setValue(DatatypeFactory.newInstance()
                                .newXMLGregorianCalendar(calendar).toXMLFormat());
                    } catch (final DatatypeConfigurationException e) {
                        this.log.log(Level.WARNING, "Error converting a date to XML.", e);
                    }

                } else {
                    responseProcessVariable.setValue(variableValue != null ? variableValue.toString() : "");
                }
                responseProcessVariables.getVariable().add(responseProcessVariable);
            }
        }

        return response;
    }

}
