/**
 * Copyright (c) 2017-2021 Linagora
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

import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_GETTASKS;

import java.util.List;
import java.util.logging.LogRecord;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

import org.flowable.job.api.DeadLetterJobQuery;
import org.flowable.job.api.Job;
import org.junit.Ignore;
import org.junit.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.commons.log.FlowLogData;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.commons.log.TraceCode;
import org.ow2.petals.component.framework.junit.Message;
import org.ow2.petals.component.framework.junit.MonitLogFilter;
import org.ow2.petals.component.framework.junit.RequestMessage;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.StatusMessage;
import org.ow2.petals.component.framework.junit.helpers.MessageChecks;
import org.ow2.petals.component.framework.junit.helpers.ServiceProviderImplementation;
import org.ow2.petals.component.framework.junit.impl.message.FaultToConsumerMessage;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.component.framework.junit.impl.message.ResponseToConsumerMessage;
import org.ow2.petals.component.framework.junit.impl.message.StatusToConsumerMessage;
import org.ow2.petals.component.framework.junit.impl.message.StatusToProviderMessage;
import org.ow2.petals.component.framework.logger.ProvideFlowStepFailureLogData;
import org.ow2.petals.component.framework.logger.StepLogHelper;
import org.ow2.petals.components.flowable.generic._1.GetTasks;
import org.ow2.petals.components.flowable.generic._1.GetTasksResponse;
import org.ow2.petals.components.flowable.generic._1.Task;
import org.ow2.petals.flowable.monitoring.FlowableActivityFlowStepData;
import org.ow2.petals.se_flowable.unit_test.call_activity.archivageservice.Archiver;
import org.ow2.petals.se_flowable.unit_test.call_activity.archivageservice.ArchiverResponse;
import org.ow2.petals.se_flowable.unit_test.call_activity.archivageservice.UnknownDocument;
import org.ow2.petals.se_flowable.unit_test.call_activity.coreservice.Execute;
import org.ow2.petals.se_flowable.unit_test.call_activity.coreservice.ExecuteResponse;
import org.ow2.petals.se_flowable.unit_test.call_activity.level1.Start;
import org.ow2.petals.se_flowable.unit_test.call_activity.level1.StartResponse;
import org.ow2.petals.se_flowable.unit_test.call_activity.level1.Unlock;
import org.ow2.petals.se_flowable.unit_test.call_activity.level1.UnlockAck;

import com.ebmwebsourcing.easycommons.xml.SourceHelper;

/**
 * Unit tests about request processing of BPMN services, with a component configured with default values and a process
 * with call activity elements
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class ServiceProviderCallActivityProcessTest extends CallActivityProcessTestEnvironment {

    private static final String TECHNICAL_ERROR_MSG = "A dummy error occurs";

    private static final String CUSTOMER_ADRESS = "customer adress";

    /**
     * <p>
     * Check the message processing where: a valid request is sent to create a new process instance from web channel.
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the process instance is automatically completed with the right service call.</li>
     * </ul>
     */
    @Test
    public void nominal() throws Exception {

        // Create a new instance of the process definition
        final StringBuilder callActivityId_level1 = new StringBuilder();
        final StringBuilder callActivityId_level2 = new StringBuilder();
        {
            final Start start = new Start();
            start.setCustomer(BPMN_USER);
            start.setAddress(CUSTOMER_ADRESS);

            // Send the 1st valid request for start event 'request
            final RequestToProviderMessage request_1 = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    CALL_ACTIVITY_PROVIDER_SU, OPERATION_START, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                    toByteArray(start));

            // Assert the response of the 1st valid request
            final ServiceProviderImplementation archiverServiceImpl = this
                    .getArchiveAttachmentsServiceImpl(new ArchiverResponse(), callActivityId_level2);
            COMPONENT.sendAndCheckResponseAndSendStatus(request_1, archiverServiceImpl, new MessageChecks() {

                @Override
                public void checks(final Message message) throws Exception {
                    // Check the reply
                    final Source fault = message.getFault();
                    assertNull("Unexpected fault", (fault == null ? null : SourceHelper.toString(fault)));
                    assertNotNull("No XML payload in response", message.getPayload());
                    final Object responseObj = UNMARSHALLER.unmarshal(message.getPayload());
                    assertTrue(responseObj instanceof StartResponse);
                    final StartResponse response = (StartResponse) responseObj;
                    assertNotNull(response.getCaseFileNumber());
                    callActivityId_level1.append(response.getCaseFileNumber());
                }
            }, ExchangeStatus.DONE);
        }

        // Wait the request of the 2nd service task
        final String callActivityId_level3;
        {
            final RequestMessage coreServiceRequestMsg = COMPONENT_UNDER_TEST.pollRequestFromConsumer();
            assertNotNull("No XML payload in response", coreServiceRequestMsg.getPayload());
            final Object coreServiceRequestObj = UNMARSHALLER.unmarshal(coreServiceRequestMsg.getPayload());
            assertTrue(coreServiceRequestObj instanceof Execute);
            final Execute coreServiceRequest = (Execute) coreServiceRequestObj;
            callActivityId_level3 = coreServiceRequest.getOrderId();

            final ResponseMessage coreServiceResponseMsg = new ResponseToConsumerMessage(coreServiceRequestMsg,
                    toByteArray(new ExecuteResponse()));
            COMPONENT_UNDER_TEST.pushResponseToConsumer(coreServiceResponseMsg);
            final StatusMessage coreServiceStatusMsg = COMPONENT_UNDER_TEST.pollStatusFromConsumer();
            assertEquals(ExchangeStatus.DONE, coreServiceStatusMsg.getStatus());
        }

        this.assertProcessInstancePending(callActivityId_level1.toString(), "processLevel1");
        this.assertProcessInstancePending(callActivityId_level2.toString(), "processLevel2");
        // TODO: Investigate why the process instance level 3 is not finished here. Perhaps not yet in historic part
        // assertProcessInstanceFinished(callActivityId_level3);
        this.waitUserTaskAssignment(callActivityId_level2.toString(), "usertask1", BPMN_USER);
        this.assertCurrentUserTask(callActivityId_level2.toString(), "usertask1", BPMN_USER);

        // Retrieve the user task basket using integration service
        {
            final GetTasks getTasksReq = new GetTasks();
            getTasksReq.setActive(true);
            getTasksReq.setAssignee(BPMN_USER);
            // getTasksReq.setProcessInstanceIdentifier(processInstanceId);

            final RequestToProviderMessage requestM = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    NATIVE_TASKS_SVC_CFG, ITG_OP_GETTASKS, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                    toByteArray(getTasksReq));

            final ResponseMessage getTaskRespMsg = COMPONENT.sendAndGetResponse(requestM);
            assertNotNull("No XML payload in response", getTaskRespMsg.getPayload());
            final Object getTaskRespObj = UNMARSHALLER.unmarshal(getTaskRespMsg.getPayload());
            assertTrue(getTaskRespObj instanceof GetTasksResponse);
            final GetTasksResponse getTaskResp = (GetTasksResponse) getTaskRespObj;
            assertNotNull(getTaskResp.getTasks());
            assertNotNull(getTaskResp.getTasks().getTask());
            assertEquals(1, getTaskResp.getTasks().getTask().size());
            final Task task = getTaskResp.getTasks().getTask().get(0);
            assertEquals("processLevel2", task.getProcessDefinitionIdentifier());
            assertEquals(callActivityId_level2.toString(), task.getProcessInstanceIdentifier());
            assertEquals("usertask1", task.getTaskIdentifier());

            COMPONENT.sendDoneStatus(getTaskRespMsg);
        }

        // Complete the user task
        {
            final Unlock userTaskRequest = new Unlock();
            userTaskRequest.setCallActivityId(callActivityId_level2.toString());
            userTaskRequest.setUnlocker(BPMN_USER);
            final RequestToProviderMessage userTaskRequestMsg = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    CALL_ACTIVITY_PROVIDER_SU, OPERATION_UNLOCK, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                    toByteArray(userTaskRequest));
            final ResponseMessage userTaskResponseMsg = COMPONENT.sendAndGetResponse(userTaskRequestMsg);
            COMPONENT.sendStatus(new StatusToProviderMessage(userTaskResponseMsg, ExchangeStatus.DONE), false);

            // Check the reply
            assertNull("Unexpected fault",
                    (userTaskResponseMsg.isFault() ? SourceHelper.toString(userTaskResponseMsg.getFault()) : null));
            assertNotNull("No XML payload in response", userTaskResponseMsg.getPayload());
            final Object userTaskResponseObj = UNMARSHALLER.unmarshal(userTaskResponseMsg.getPayload());
            assertTrue(userTaskResponseObj instanceof UnlockAck);
        }

        // Wait the end of the process instance
        this.waitEndOfProcessInstance(callActivityId_level1.toString());

        // Assertions about state of process instance at Flowable Level
        this.assertProcessInstanceFinished(callActivityId_level1.toString());

        // Check MONIT traces
        final List<LogRecord> allMonitLogs = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        final LogRecord firstLogOfProcessService = new MonitLogFilter(allMonitLogs)
                .traceCode(TraceCode.PROVIDE_FLOW_STEP_BEGIN)
                .interfaceName(
                        new QName("http://petals.ow2.org/se-flowable/unit-test/call-activity/level1", "call-activity"))
                .serviceName(new QName("http://petals.ow2.org/se-flowable/unit-test/call-activity/level1",
                        "call-activity-service"))
                .operationName(new QName("http://petals.ow2.org/se-flowable/unit-test/call-activity/level1", "start"))
                .singleResult();
        final String flowInstanceIdProcessService = (String) ((FlowLogData) firstLogOfProcessService.getParameters()[0])
                .get(FlowLogData.FLOW_INSTANCE_ID_PROPERTY_NAME);
        final LogRecord firstLogOfProcess = new MonitLogFilter(allMonitLogs)
                .traceCode(TraceCode.CONSUME_EXT_FLOW_STEP_BEGIN)
                .property(FlowableActivityFlowStepData.CORRELATED_FLOW_INSTANCE_ID_KEY, flowInstanceIdProcessService)
                .singleResult();
        final String flowInstanceIdProcess = (String) ((FlowLogData) firstLogOfProcess.getParameters()[0])
                .get(FlowLogData.FLOW_INSTANCE_ID_PROPERTY_NAME);
        final List<LogRecord> processMonitLogs = new MonitLogFilter(allMonitLogs).flowInstanceId(flowInstanceIdProcess)
                .results();
        
        assertEquals(12, processMonitLogs.size());
        final FlowLogData initialProcessFlowLogData = assertMonitConsumerExtBeginLog(processMonitLogs.get(0));
        final String processInstanceId = (String) initialProcessFlowLogData
                .get(FlowableActivityFlowStepData.PROCESS_INSTANCE_ID_KEY);
        assertNotNull("process instance id missing in log trace", processInstanceId);

        final FlowLogData firstCallActivity = assertMonitProviderBeginLog(initialProcessFlowLogData, null, null, null,
                null, processMonitLogs.get(1));
        assertEquals(processInstanceId, firstCallActivity.get(FlowableActivityFlowStepData.PROCESS_INSTANCE_ID_KEY));
        assertEquals("processLevel2", firstCallActivity.get(FlowableActivityFlowStepData.CALL_ACTIVITY_DEFINITION_KEY));
        final String callActivityInstanceId_1 = (String) firstCallActivity
                .get(FlowableActivityFlowStepData.CALL_ACTIVITY_INSTANCE_ID_KEY);
        assertNotNull("call activity instance id missing in log trace", callActivityInstanceId_1);

        final FlowLogData archiverFlowLogData = assertMonitProviderBeginLog(firstCallActivity, ARCHIVE_INTERFACE,
                ARCHIVE_SERVICE, ARCHIVE_ENDPOINT, ARCHIVER_OPERATION, processMonitLogs.get(2));
        assertMonitProviderEndLog(archiverFlowLogData, processMonitLogs.get(3));

        // TODO: Uncomment following block when problem about flowStepId/previousFlowStepId on two consecutive
        // service tasks will be checked. See:
        // https://cdeneux.framaboard.org/?controller=TaskViewController&action=show&task_id=251&project_id=1
        /*
        final FlowLogData secondCallActivity = assertMonitProviderBeginLog(archiverFlowLogData, null, null, null,
                null, processMonitLogs.get(4));
        assertEquals(callActivityInstanceId_1,
                secondCallActivity.get(FlowableActivityFlowStepData.PROCESS_INSTANCE_ID_KEY));
        assertEquals("processLevel3",
                secondCallActivity.get(FlowableActivityFlowStepData.CALL_ACTIVITY_DEFINITION_KEY));
        final String callActivityInstanceId_2 = (String) secondCallActivity
                .get(FlowableActivityFlowStepData.CALL_ACTIVITY_INSTANCE_ID_KEY);
        assertNotNull("call activity instance id missing in log trace", callActivityInstanceId_2);

        assertMonitProviderEndLog(assertMonitProviderBeginLog(secondCallActivity, CORE_SVC_INTERFACE, CORE_SVC_SERVICE,
                CORE_SVC_ENDPOINT, CORE_SVC_OPERATION, processMonitLogs.get(5)), processMonitLogs.get(6));
        */

    }

    /**
     * <p>
     * Check the processing of an error returned by a Petals service invoked from a service task.
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the error is correctly pushed to the Flowable engine as a technical error,</li>
     * <li>the service task is retried several times.</li>
     * </ul>
     */
    @Test
    public void jbiErrorOnServiceTask() throws Exception {

        // Create a new instance of the process definition
        final StringBuilder processInstance_level1 = new StringBuilder();
        final StringBuilder callActivityId_level2 = new StringBuilder();
        {
            final Start start = new Start();
            start.setCustomer(BPMN_USER);
            start.setAddress(CUSTOMER_ADRESS);

            // Send the 1st valid request for start event 'request
            final RequestToProviderMessage request_1 = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    CALL_ACTIVITY_PROVIDER_SU, OPERATION_START, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                    toByteArray(start));

            // Assert the response of the 1st valid request
            final ServiceProviderImplementation archiverServiceImpl = this
                    .getArchiveAttachmentsServiceImplAsError(new ArchiverResponse(), callActivityId_level2);
            COMPONENT.sendAndCheckResponseAndSendStatus(request_1, archiverServiceImpl, new MessageChecks() {

                @Override
                public void checks(final Message message) throws Exception {
                    // Check the reply
                    final Source fault = message.getFault();
                    assertNull("Unexpected fault", (fault == null ? null : SourceHelper.toString(fault)));
                    assertNotNull("No XML payload in response", message.getPayload());
                    final Object responseObj = UNMARSHALLER.unmarshal(message.getPayload());
                    assertTrue(responseObj instanceof StartResponse);
                    final StartResponse response = (StartResponse) responseObj;
                    assertNotNull(response.getCaseFileNumber());
                    processInstance_level1.append(response.getCaseFileNumber());
                }
            }, ExchangeStatus.DONE);
        }

        // 2nd service task invocation try
        {
            final RequestMessage requestMsg2 = COMPONENT_UNDER_TEST.pollRequestFromConsumer();
            assertIsArchiveServiceRequest(requestMsg2.getMessageExchange());
            COMPONENT_UNDER_TEST.pushStatusToConsumer(
                    new StatusToConsumerMessage(requestMsg2, new Exception("A dummy error occurs")));
        }

        // 3nd service task invocation try
        {
            final RequestMessage requestMsg3 = COMPONENT_UNDER_TEST.pollRequestFromConsumer();
            assertIsArchiveServiceRequest(requestMsg3.getMessageExchange());
            COMPONENT_UNDER_TEST.pushStatusToConsumer(
                    new StatusToConsumerMessage(requestMsg3, new Exception("A dummy error occurs")));
        }

        // Wait that the process instance is moved as a dead letter
        this.waitProcessInstanceAsDeadLetterJob(callActivityId_level2.toString());

        // Resume the dead letter job
        final DeadLetterJobQuery deadLetterJobQuery = this.flowableClient.getManagementService()
                .createDeadLetterJobQuery().processInstanceId(callActivityId_level2.toString());
        final Job deadLetterJob = deadLetterJobQuery.singleResult();
        assertNotNull(deadLetterJob);
        this.flowableClient.getManagementService().moveDeadLetterJobToExecutableJob(deadLetterJob.getId(), 2);

        // service task invocation tries after re-activation
        for (int i = 0; i < 2; i++) {
            final RequestMessage requestMsg = COMPONENT_UNDER_TEST.pollRequestFromConsumer();
            assertIsArchiveServiceRequest(requestMsg.getMessageExchange());
            COMPONENT_UNDER_TEST.pushStatusToConsumer(
                    new StatusToConsumerMessage(requestMsg, new Exception("A dummy error occurs")));
        }

        // Wait that the process instance is moved as a dead letter
        this.waitProcessInstanceAsDeadLetterJob(callActivityId_level2.toString());

        // Check MONIT traces
        final List<LogRecord> allMonitLogs = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        final LogRecord firstLogOfProcessService = new MonitLogFilter(allMonitLogs)
                .traceCode(TraceCode.PROVIDE_FLOW_STEP_BEGIN)
                .interfaceName(
                        new QName("http://petals.ow2.org/se-flowable/unit-test/call-activity/level1", "call-activity"))
                .serviceName(new QName("http://petals.ow2.org/se-flowable/unit-test/call-activity/level1",
                        "call-activity-service"))
                .operationName(new QName("http://petals.ow2.org/se-flowable/unit-test/call-activity/level1", "start"))
                .singleResult();
        final String flowInstanceIdProcessService = (String) ((FlowLogData) firstLogOfProcessService.getParameters()[0])
                .get(FlowLogData.FLOW_INSTANCE_ID_PROPERTY_NAME);
        final LogRecord firstLogOfProcess = new MonitLogFilter(allMonitLogs)
                .traceCode(TraceCode.CONSUME_EXT_FLOW_STEP_BEGIN)
                .property(FlowableActivityFlowStepData.CORRELATED_FLOW_INSTANCE_ID_KEY, flowInstanceIdProcessService)
                .singleResult();
        final String flowInstanceIdProcess = (String) ((FlowLogData) firstLogOfProcess.getParameters()[0])
                .get(FlowLogData.FLOW_INSTANCE_ID_PROPERTY_NAME);
        final List<LogRecord> processMonitLogs = new MonitLogFilter(allMonitLogs).flowInstanceId(flowInstanceIdProcess)
                .results();

        assertEquals(12, processMonitLogs.size());
        final FlowLogData initialProcessFlowLogData = assertMonitConsumerExtBeginLog(processMonitLogs.get(0));
        assertEquals("process instance id missing in log trace", processInstance_level1.toString(), initialProcessFlowLogData
                        .get(FlowableActivityFlowStepData.PROCESS_INSTANCE_ID_KEY));
        final FlowLogData firstCallActivity = assertMonitProviderBeginLog(initialProcessFlowLogData, null, null, null,
                null, processMonitLogs.get(1));
        assertEquals(processInstance_level1.toString(),
                firstCallActivity.get(FlowableActivityFlowStepData.PROCESS_INSTANCE_ID_KEY));
        assertEquals("processLevel2", firstCallActivity.get(FlowableActivityFlowStepData.CALL_ACTIVITY_DEFINITION_KEY));
        assertEquals("call activity instance id missing in log trace", callActivityId_level2.toString(),
                firstCallActivity.get(FlowableActivityFlowStepData.CALL_ACTIVITY_INSTANCE_ID_KEY));

        for (int i = 0; i < 5; i++) {
            final FlowLogData archiverFlowLogData = assertMonitProviderBeginLog(firstCallActivity, ARCHIVE_INTERFACE,
                    ARCHIVE_SERVICE, ARCHIVE_ENDPOINT, ARCHIVER_OPERATION, processMonitLogs.get(2 + 2 * i));
            final FlowLogData archiverFlowLogDataFailure = assertMonitProviderFailureLog(archiverFlowLogData,
                    processMonitLogs.get(3 + 2 * i));
            assertEquals("Unexpected error message", TECHNICAL_ERROR_MSG,
                    archiverFlowLogDataFailure.get(ProvideFlowStepFailureLogData.FLOW_STEP_FAILURE_MESSAGE_NAME));
        }

    }

    /**
     * <p>
     * Check the processing of a fault returned by a Petals service invoked from a service task.
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the JBI fault is correctly pushed to the Flowable engine as a BPMN error,</li>
     * <li>the service task is executed only once.</li>
     * </ul>
     */
    @Test
    @Ignore("Until the call activity completion/cancelation is not correctly trapped ! (PETALSSEFLOWABLE-35)")
    public void jbiFaultOnServiceTask() throws Exception {

        // Create a new instance of the process definition
        final StringBuilder processInstance_level1 = new StringBuilder();
        final StringBuilder callActivityId_level2 = new StringBuilder();
        {
            final Start start = new Start();
            start.setCustomer(BPMN_USER);
            start.setAddress(CUSTOMER_ADRESS);

            // Send the 1st valid request for start event 'request
            final RequestToProviderMessage request_1 = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    CALL_ACTIVITY_PROVIDER_SU, OPERATION_START, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                    toByteArray(start));

            // Assert the response of the 1st valid request
            final ServiceProviderImplementation archiverServiceImpl = this
                    .getArchiveAttachmentsServiceImplAsFault(new ArchiverResponse(), callActivityId_level2);
            COMPONENT.sendAndCheckResponseAndSendStatus(request_1, archiverServiceImpl, new MessageChecks() {

                @Override
                public void checks(final Message message) throws Exception {
                    // Check the reply
                    final Source fault = message.getFault();
                    assertNull("Unexpected fault", (fault == null ? null : SourceHelper.toString(fault)));
                    assertNotNull("No XML payload in response", message.getPayload());
                    final Object responseObj = UNMARSHALLER.unmarshal(message.getPayload());
                    assertTrue(responseObj instanceof StartResponse);
                    final StartResponse response = (StartResponse) responseObj;
                    assertNotNull(response.getCaseFileNumber());
                    processInstance_level1.append(response.getCaseFileNumber());
                }
            }, ExchangeStatus.DONE);
        }

        // Wait that the process instance ends
        this.waitEndOfProcessInstance(callActivityId_level2.toString());

        // Check MONIT traces
        final List<LogRecord> allMonitLogs = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        final LogRecord firstLogOfProcessService = new MonitLogFilter(allMonitLogs)
                .traceCode(TraceCode.PROVIDE_FLOW_STEP_BEGIN)
                .interfaceName(
                        new QName("http://petals.ow2.org/se-flowable/unit-test/call-activity/level1", "call-activity"))
                .serviceName(new QName("http://petals.ow2.org/se-flowable/unit-test/call-activity/level1",
                        "call-activity-service"))
                .operationName(new QName("http://petals.ow2.org/se-flowable/unit-test/call-activity/level1", "start"))
                .singleResult();
        final String flowInstanceIdProcessService = (String) ((FlowLogData) firstLogOfProcessService.getParameters()[0])
                .get(FlowLogData.FLOW_INSTANCE_ID_PROPERTY_NAME);
        final LogRecord firstLogOfProcess = new MonitLogFilter(allMonitLogs)
                .traceCode(TraceCode.CONSUME_EXT_FLOW_STEP_BEGIN)
                .property(FlowableActivityFlowStepData.CORRELATED_FLOW_INSTANCE_ID_KEY, flowInstanceIdProcessService)
                .singleResult();
        final String flowInstanceIdProcess = (String) ((FlowLogData) firstLogOfProcess.getParameters()[0])
                .get(FlowLogData.FLOW_INSTANCE_ID_PROPERTY_NAME);
        final List<LogRecord> processMonitLogs = new MonitLogFilter(allMonitLogs).flowInstanceId(flowInstanceIdProcess)
                .results();

        assertEquals(6, processMonitLogs.size());
        final FlowLogData initialProcessFlowLogData = assertMonitConsumerExtBeginLog(processMonitLogs.get(0));
        assertEquals("process instance id missing in log trace", processInstance_level1.toString(),
                initialProcessFlowLogData.get(FlowableActivityFlowStepData.PROCESS_INSTANCE_ID_KEY));
        final FlowLogData firstCallActivity = assertMonitProviderBeginLog(initialProcessFlowLogData, null, null, null,
                null, processMonitLogs.get(1));
        assertEquals(processInstance_level1.toString(),
                firstCallActivity.get(FlowableActivityFlowStepData.PROCESS_INSTANCE_ID_KEY));
        assertEquals("processLevel2", firstCallActivity.get(FlowableActivityFlowStepData.CALL_ACTIVITY_DEFINITION_KEY));
        assertEquals("call activity instance id missing in log trace", callActivityId_level2.toString(),
                firstCallActivity.get(FlowableActivityFlowStepData.CALL_ACTIVITY_INSTANCE_ID_KEY));

        final FlowLogData archiverFlowLogData = assertMonitProviderBeginLog(firstCallActivity, ARCHIVE_INTERFACE,
                ARCHIVE_SERVICE, ARCHIVE_ENDPOINT, ARCHIVER_OPERATION, processMonitLogs.get(2));
        final FlowLogData archiverFlowLogDataFailure = assertMonitProviderFailureLog(archiverFlowLogData,
                processMonitLogs.get(3));
        assertEquals("Unexpected error message", StepLogHelper.BUSINESS_ERROR_MESSAGE,
                archiverFlowLogDataFailure.get(ProvideFlowStepFailureLogData.FLOW_STEP_FAILURE_MESSAGE_NAME));

        // This log should be a consulerExtFailureLog
        assertMonitConsumerExtEndLog(firstCallActivity, processMonitLogs.get(4));
        assertMonitConsumerExtEndLog(initialProcessFlowLogData, processMonitLogs.get(5));


    }

    private ServiceProviderImplementation getArchiveAttachmentsServiceImpl(final ArchiverResponse responseBean,
            final StringBuilder callActivityId_level1) {

        return new ServiceProviderImplementation() {
            private MessageExchange msgExchange;

            @Override
            public Message provides(final RequestMessage requestMsg) throws Exception {

                this.msgExchange = requestMsg.getMessageExchange();
                assertIsArchiveServiceRequest(this.msgExchange);
                final Object requestObj = UNMARSHALLER.unmarshal(requestMsg.getPayload());
                assertTrue(requestObj instanceof Archiver);
                callActivityId_level1.append(((Archiver) requestObj).getItem());

                // Returns the reply of the service provider to the Flowable service task
                return new ResponseToConsumerMessage(requestMsg, toByteArray(responseBean));
            }

            @Override
            public void handleStatus(final StatusMessage statusDoneMsg) throws Exception {
                // Assert the status DONE on the message exchange
                assertNotNull(statusDoneMsg);
                // It's the same message exchange instance
                assertSame(statusDoneMsg.getMessageExchange(), this.msgExchange);
                assertEquals(statusDoneMsg.getMessageExchange().getStatus(), ExchangeStatus.DONE);
            }
        };
    }

    private ServiceProviderImplementation getArchiveAttachmentsServiceImplAsError(final ArchiverResponse responseBean,
            final StringBuilder callActivityId_level1) {

        return new ServiceProviderImplementation() {

            @Override
            public Message provides(final RequestMessage requestMsg) throws Exception {

                assertIsArchiveServiceRequest(requestMsg.getMessageExchange());
                final Object requestObj = UNMARSHALLER.unmarshal(requestMsg.getPayload());
                assertTrue(requestObj instanceof Archiver);
                callActivityId_level1.append(((Archiver) requestObj).getItem());

                // Returns an error to the Flowable service task
                return new StatusToConsumerMessage(requestMsg, new Exception(TECHNICAL_ERROR_MSG));
            }

            @Override
            public boolean statusExpected() {
                return false;
            }
        };
    }

    private ServiceProviderImplementation getArchiveAttachmentsServiceImplAsFault(final ArchiverResponse responseBean,
            final StringBuilder callActivityId_level1) {

        return new ServiceProviderImplementation() {
            private MessageExchange msgExchange;

            @Override
            public Message provides(final RequestMessage requestMsg) throws Exception {

                this.msgExchange = requestMsg.getMessageExchange();
                assertIsArchiveServiceRequest(this.msgExchange);
                final Object requestObj = UNMARSHALLER.unmarshal(requestMsg.getPayload());
                assertTrue(requestObj instanceof Archiver);
                callActivityId_level1.append(((Archiver) requestObj).getItem());

                // Returns a fault to the Flowable service task
                final UnknownDocument archiveFault = new UnknownDocument();
                archiveFault.setDocId("docId");
                return new FaultToConsumerMessage(requestMsg, toByteArray(archiveFault));
            }

            @Override
            public void handleStatus(final StatusMessage statusDoneMsg) throws Exception {
                // Assert the status DONE on the message exchange
                assertNotNull(statusDoneMsg);
                // It's the same message exchange instance
                assertSame(statusDoneMsg.getMessageExchange(), this.msgExchange);
                assertEquals(statusDoneMsg.getMessageExchange().getStatus(), ExchangeStatus.DONE);
            }
        };
    }

    private static void assertIsArchiveServiceRequest(final MessageExchange msgExchange) {
        assertNotNull(msgExchange);
        assertEquals(ARCHIVE_INTERFACE, msgExchange.getInterfaceName());
        assertEquals(ARCHIVE_SERVICE, msgExchange.getService());
        assertNotNull(msgExchange.getEndpoint());
        assertEquals(ARCHIVE_ENDPOINT, msgExchange.getEndpoint().getEndpointName());
        assertEquals(ARCHIVER_OPERATION, msgExchange.getOperation());
        assertEquals(msgExchange.getStatus(), ExchangeStatus.ACTIVE);
    }
}
