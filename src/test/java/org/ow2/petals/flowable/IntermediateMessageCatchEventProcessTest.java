/**
 * Copyright (c) 2017-2020 Linagora
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
package org.ow2.petals.flowable;

import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_EXECUTIONS_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_EXECUTIONS_SERVICE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_GETEXECUTIONS;

import java.util.List;
import java.util.logging.LogRecord;

import javax.jbi.messaging.ExchangeStatus;
import javax.xml.transform.Source;

import org.flowable.engine.repository.ProcessDefinition;
import org.flowable.engine.runtime.Execution;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.task.api.Task;
import org.junit.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.commons.log.FlowLogData;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.StatusMessage;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.components.flowable.generic._1.GetExecutions;
import org.ow2.petals.components.flowable.generic._1.GetExecutionsResponse;
import org.ow2.petals.flowable.incoming.operation.exception.UnexpectedMessageEventException;
import org.ow2.petals.flowable.monitoring.IntermediateCatchMessageEventFlowStepBeginLogData;
import org.ow2.petals.flowable.utils.test.Await;
import org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event.NotLocked;
import org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event.Start;
import org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event.StartResponse;
import org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event.Unlock;

import com.ebmwebsourcing.easycommons.xml.SourceHelper;

/**
 * Unit tests about request processing of BPMN services, with a component configured with default values and a process
 * with call a BPMN element 'intermediate message catch event'
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class IntermediateMessageCatchEventProcessTest extends IntermediateMessageCatchEventProcessTestEnvironment {

    private static final String VARIABLE_1_VALUE = "variable-1-value";

    private static final String VARIABLE_2_VALUE = "variable-2-value";

    /**
     * <p>
     * Check the message processing where: a valid request is sent to event receipt.
     * </p>
     * <p>
     * Note: A first process instance is completed to be sure the historic is not empty, to check if error can occurs on
     * historic query returning more than one result
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>if the event is received when not expected by the BPMN engine, an error is returned,</li>
     * <li>if the event is received when expected by the BPMN engine, no error occurs and the process execution
     * continues,</li>
     * <li>if the event is received when already received by the BPMN engine, an error is returned,</li>
     * <li>the process instance is automatically completed when all is correctly done.</li>
     * </ul>
     */
    @Test
    public void execute() throws Exception {

        // We add a completed process instance to check if error can occurs on historic query returning more than one
        // result.
        this.createCompletedProcessInstance();

        // ------------------------------------------------------------
        // Create a new instance of the process definition:
        // - After creation, the process instance is waiting a user task completion
        // ------------------------------------------------------------

        // Create a new instance of the process definition
        final StringBuilder processInstanceId = new StringBuilder();
        {
            final Start start = new Start();
            start.setCustomer(BPMN_USER);

            // Send the 1st valid request for start event 'request
            final RequestToProviderMessage request_1 = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, OPERATION_START,
                    AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                    toByteArray(start));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request_1);
            final ResponseMessage response_1 = COMPONENT_UNDER_TEST.pollResponseFromProvider();
            final Source fault = response_1.getFault();
            assertNull("Unexpected fault", (fault == null ? null : SourceHelper.toString(fault)));
            assertNotNull("No XML payload in response", response_1.getPayload());
            final Object responseObj = UNMARSHALLER.unmarshal(response_1.getPayload());
            assertTrue(responseObj instanceof StartResponse);
            final StartResponse response = (StartResponse) responseObj;
            assertNotNull(response.getCaseFileNumber());
            processInstanceId.append(response.getCaseFileNumber());
        }

        this.assertProcessInstancePending(processInstanceId.toString(), BPMN_PROCESS_DEFINITION_KEY);
        this.waitUserTaskAssignment(processInstanceId.toString(), USER_TASK_1, BPMN_USER);

        // ----------------------------------------------------------------------------
        // Send the intermediate message event when not expected by the BPMN engine
        // - Expected status ERROR with MEP InOnly
        // - Expected fault with MEP RobustInOnly
        // ----------------------------------------------------------------------------
        {
            final Unlock unlockRequest = new Unlock();
            unlockRequest.setInstanceId(processInstanceId.toString());
            unlockRequest.setVar1(VARIABLE_1_VALUE);
            unlockRequest.setVar2(VARIABLE_2_VALUE);
            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, OPERATION_UNLOCK,
                    AbsItfOperation.MEPPatternConstants.IN_ONLY.value(), toByteArray(unlockRequest));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request);
            final StatusMessage response = COMPONENT_UNDER_TEST.pollStatusFromProvider();
            assertEquals(ExchangeStatus.ERROR, response.getStatus());
            assertTrue(response.getError() instanceof UnexpectedMessageEventException);
        }
        {
            final Unlock unlockRequest = new Unlock();
            unlockRequest.setInstanceId(processInstanceId.toString());

            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, OPERATION_UNLOCK,
                    AbsItfOperation.MEPPatternConstants.ROBUST_IN_ONLY.value(), toByteArray(unlockRequest));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request);
            final ResponseMessage response = COMPONENT_UNDER_TEST.pollResponseFromProvider();
            assertEquals(ExchangeStatus.ACTIVE, response.getStatus());
            final Source fault = response.getFault();
            assertNotNull("No fault returns", fault);
            final Object responseObj = UNMARSHALLER.unmarshal(fault);
            assertTrue(responseObj instanceof NotLocked);
            final NotLocked responseBean = (NotLocked) responseObj;
            assertEquals(processInstanceId.toString(), responseBean.getInstanceId());
            assertEquals(MESSAGE_EVENT_NAME, responseBean.getEventName());
        }

        this.assertProcessInstancePending(processInstanceId.toString(), BPMN_PROCESS_DEFINITION_KEY);
        this.waitUserTaskAssignment(processInstanceId.toString(), USER_TASK_1, BPMN_USER);

        // Complete the user task
        IN_MEMORY_LOG_HANDLER.clear();
        this.flowableClient.completeUserTask(processInstanceId.toString(), USER_TASK_1, BPMN_USER);
        this.assertUserTaskEnded(processInstanceId.toString(), USER_TASK_1, BPMN_USER);
        this.waitIntermediateCatchMessageEvent(processInstanceId.toString(), MESSAGE_EVENT_NAME);

        // ---------------------------------------------------------------------------------------------
        // Try to retrieve the execution waiting the message event using the right integration service
        // ---------------------------------------------------------------------------------------------
        {
            final GetExecutions getExecutionsReq = new GetExecutions();
            getExecutionsReq.setProcessDefinitionIdentifier(BPMN_PROCESS_DEFINITION_KEY);
            getExecutionsReq.setProcessInstanceIdentifier(processInstanceId.toString());
            getExecutionsReq.setEventName(MESSAGE_EVENT_NAME);
            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    NATIVE_EXECUTIONS_SVC_CFG, ITG_OP_GETEXECUTIONS, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                    toByteArray(getExecutionsReq));
            final ResponseMessage getExecutionsRespMsg = COMPONENT.sendAndGetResponse(request);
            assertNotNull("No XML payload in response", getExecutionsRespMsg.getPayload());
            final Object getExecutionsRespObj = UNMARSHALLER.unmarshal(getExecutionsRespMsg.getPayload());
            assertTrue(getExecutionsRespObj instanceof GetExecutionsResponse);
            final GetExecutionsResponse getExecutionsResp = (GetExecutionsResponse) getExecutionsRespObj;
            assertNotNull(getExecutionsResp.getExecutions());
            assertNotNull(getExecutionsResp.getExecutions().getExecution());
            assertEquals(1, getExecutionsResp.getExecutions().getExecution().size());
            assertEquals(processInstanceId.toString(),
                    getExecutionsResp.getExecutions().getExecution().get(0).getProcessInstanceIdentifier());
        }

        // ----------------------------------------------------------------------------
        // Send the intermediate message event when expected by the BPMN engine
        // ----------------------------------------------------------------------------
        {
            final Unlock unlockRequest = new Unlock();
            unlockRequest.setInstanceId(processInstanceId.toString());
            unlockRequest.setVar1(VARIABLE_1_VALUE);
            unlockRequest.setVar2(VARIABLE_2_VALUE);

            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, OPERATION_UNLOCK,
                    AbsItfOperation.MEPPatternConstants.ROBUST_IN_ONLY.value(), toByteArray(unlockRequest));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request);
            final StatusMessage response = COMPONENT_UNDER_TEST.pollStatusFromProvider();
            assertEquals(ExchangeStatus.DONE, response.getStatus());
        }

        this.assertProcessInstancePending(processInstanceId.toString(), BPMN_PROCESS_DEFINITION_KEY);
        this.waitUserTaskAssignment(processInstanceId.toString(), USER_TASK_2, BPMN_USER);

        // Assert that complementary variables are correctly set
        final ProcessInstance procInst = this.flowableClient.getRuntimeService().createProcessInstanceQuery()
                .processInstanceId(processInstanceId.toString()).includeProcessVariables().singleResult();
        assertNotNull(procInst);
        assertEquals(VARIABLE_1_VALUE, procInst.getProcessVariables().get("variable-1"));
        assertEquals(VARIABLE_2_VALUE, procInst.getProcessVariables().get("variable-2"));

        // Check MONIT traces. Caution:
        // - as the user task is completed by the Flowable client, no MONIT trace is generated
        // - the last trace is associated to the next user task
        final List<LogRecord> monitLogs_1 = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(7, monitLogs_1.size());
        final FlowLogData intermediateCatchMessageEventBeginFlowLogData = assertMonitProviderBeginLog(null, null, null,
                null, monitLogs_1.get(0));
        assertEquals("messageIntermediateCatchEventId", intermediateCatchMessageEventBeginFlowLogData
                .get(IntermediateCatchMessageEventFlowStepBeginLogData.INTERMEDIATE_CATCH_MESSAGE_EVENT_ID));
        assertEquals(MESSAGE_EVENT_NAME, intermediateCatchMessageEventBeginFlowLogData
                .get(IntermediateCatchMessageEventFlowStepBeginLogData.MESSAGE_NAME));
        assertNotNull(intermediateCatchMessageEventBeginFlowLogData
                .get(IntermediateCatchMessageEventFlowStepBeginLogData.INTERMEDIATE_CATCH_MESSAGE_EVENT_INSTANCE_ID));
        final FlowLogData getExecutionsRequestFlowLogData = assertMonitProviderBeginLog(ITG_EXECUTIONS_PORT_TYPE,
                ITG_EXECUTIONS_SERVICE, COMPONENT_UNDER_TEST.getNativeEndpointName(ITG_EXECUTIONS_SERVICE),
                ITG_OP_GETEXECUTIONS, monitLogs_1.get(1));
        assertMonitProviderEndLog(getExecutionsRequestFlowLogData, monitLogs_1.get(2));
        final FlowLogData unlockRequestFlowLogData = assertMonitProviderBeginLog(
                INTERMEDIATE_MESSAGE_CATCH_EVENT_INTERFACE, INTERMEDIATE_MESSAGE_CATCH_EVENT_SERVICE,
                INTERMEDIATE_MESSAGE_CATCH_EVENT_ENDPOINT, OPERATION_UNLOCK,
                monitLogs_1.get(3));
        assertMonitProviderEndLog(unlockRequestFlowLogData, monitLogs_1.get(4));
        assertMonitProviderEndLog(intermediateCatchMessageEventBeginFlowLogData, unlockRequestFlowLogData,
                monitLogs_1.get(5));

        // ----------------------------------------------------------------------------
        // Send the intermediate message event when it was already processed by the BPMN engine
        // - Expected status ERROR with MEP InOnly
        // - Expected fault with MEP RobustInOnly
        // ----------------------------------------------------------------------------
        {
            final Unlock unlockRequest = new Unlock();
            unlockRequest.setInstanceId(processInstanceId.toString());

            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, OPERATION_UNLOCK,
                    AbsItfOperation.MEPPatternConstants.IN_ONLY.value(), toByteArray(unlockRequest));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request);
            final StatusMessage response = COMPONENT_UNDER_TEST.pollStatusFromProvider();
            assertEquals(ExchangeStatus.ERROR, response.getStatus());
            assertTrue(response.getError() instanceof UnexpectedMessageEventException);
        }
        {
            final Unlock unlockRequest = new Unlock();
            unlockRequest.setInstanceId(processInstanceId.toString());

            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, OPERATION_UNLOCK,
                    AbsItfOperation.MEPPatternConstants.ROBUST_IN_ONLY.value(), toByteArray(unlockRequest));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request);
            final ResponseMessage response = COMPONENT_UNDER_TEST.pollResponseFromProvider();
            assertEquals(ExchangeStatus.ACTIVE, response.getStatus());
            final Source fault = response.getFault();
            assertNotNull("No fault returns", fault);
            final Object responseObj = UNMARSHALLER.unmarshal(fault);
            assertTrue(responseObj instanceof NotLocked);
            final NotLocked responseBean = (NotLocked) responseObj;
            assertEquals(processInstanceId.toString(), responseBean.getInstanceId());
            assertEquals(MESSAGE_EVENT_NAME, responseBean.getEventName());
        }

        // Complete the 2nd user task
        this.flowableClient.completeUserTask(processInstanceId.toString(), USER_TASK_2, BPMN_USER);
        this.assertUserTaskEnded(processInstanceId.toString(), USER_TASK_2, BPMN_USER);

        // Wait the end of the process instance
        this.waitEndOfProcessInstance(processInstanceId.toString());

        // Assertions about state of process instance at Flowable Level
        this.assertProcessInstanceFinished(processInstanceId.toString());

    }

    private void createCompletedProcessInstance() throws InterruptedException {

        final ProcessDefinition deployment = this.flowableClient.getRepositoryService().createProcessDefinitionQuery()
                .processDefinitionKey("intermediate-message-catch-event").singleResult();
        final ProcessInstance procInst = this.flowableClient.getRuntimeService()
                .startProcessInstanceById(deployment.getId());
        Task task = this.flowableClient.getTaskService().createTaskQuery().processInstanceId(procInst.getId())
                .singleResult();
        this.flowableClient.getTaskService().complete(task.getId());

        Await.waitIntermediateCatchMessageEvent(procInst.getId(), "myMessageName",
                this.flowableClient.getRuntimeService());
        final Execution execution = this.flowableClient.getRuntimeService().createExecutionQuery()
                .processInstanceId(procInst.getId()).activityId("messageIntermediateCatchEventId")
                .messageEventSubscriptionName("myMessageName").singleResult();
        this.flowableClient.getRuntimeService().messageEventReceived("myMessageName", execution.getId());

        task = this.flowableClient.getTaskService().createTaskQuery().processInstanceId(procInst.getId())
                .singleResult();
        this.flowableClient.getTaskService().complete(task.getId());

        assertEquals(1, this.flowableClient.getHistoryService().createHistoricProcessInstanceQuery()
                .processInstanceId(procInst.getId()).finished().count());

        IN_MEMORY_LOG_HANDLER.clear();

    }
}
