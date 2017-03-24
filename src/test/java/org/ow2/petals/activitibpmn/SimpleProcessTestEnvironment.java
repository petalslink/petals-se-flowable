/**
 * Copyright (c) 2014-2017 Linagora
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
package org.ow2.petals.activitibpmn;

import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_PORT_TYPE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_SERVICE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_TASK_PORT_TYPE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_TASK_SERVICE;

import java.io.File;
import java.net.URL;

import javax.xml.namespace.QName;

import org.junit.ClassRule;
import org.junit.rules.RuleChain;
import org.junit.rules.TestRule;
import org.ow2.petals.component.framework.junit.helpers.SimpleComponent;
import org.ow2.petals.component.framework.junit.impl.ConsumesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.ComponentUnderTest;
import org.ow2.petals.component.framework.junit.rule.NativeServiceConfigurationFactory;
import org.ow2.petals.component.framework.junit.rule.ParameterGenerator;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;

/**
 * Abstract class for unit tests about request processing
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public abstract class SimpleProcessTestEnvironment extends AbstractTestEnvironment {

    private static final String VACATION_NAMESPACE = "http://petals.ow2.org/samples/se-bpmn/vacationService";

    protected static final QName VACATION_INTERFACE = new QName(VACATION_NAMESPACE, "demandeDeConges");

    protected static final QName VACATION_SERVICE = new QName(VACATION_NAMESPACE, "demandeDeCongesService");

    protected static final String VACATION_ENDPOINT = "testEndpointName";

    protected static final QName OPERATION_JIRA = new QName(VACATION_NAMESPACE, "jira_PETALSSEACTIVITI-4");

    protected static final QName OPERATION_DEMANDERCONGES = new QName(VACATION_NAMESPACE, "demanderConges");

    protected static final QName OPERATION_VALIDERDEMANDE = new QName(VACATION_NAMESPACE, "validerDemande");

    private static final String ARCHIVE_NAMESPACE = "http://petals.ow2.org/samples/se-bpmn/archivageService";

    protected static final QName ARCHIVE_INTERFACE = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static final QName ARCHIVE_SERVICE = new QName(ARCHIVE_NAMESPACE, "archiverService");

    protected static final String ARCHIVE_ENDPOINT = "archiveEndpointName";

    protected static final QName ARCHIVER_OPERATION = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static final String NATIVE_TASKS_SVC_CFG = "native-tasks";

    protected static final String NATIVE_PROCESSINSTANCES_SVC_CFG = "native-process-instances";

    protected static final String BPMN_PROCESS_DEFINITION_KEY = "vacationRequest";

    protected static final String BPMN_PROCESS_1ST_USER_TASK_KEY = "handleRequest";

    protected static final String BPMN_PROCESS_2ND_USER_TASK_KEY = "adjustVacationRequestTask";

    protected static final String BPMN_USER_DEMANDEUR = "demandeur";

    protected static final String BPMN_USER_VALIDEUR = "valideur";

    protected static final ComponentUnderTest COMPONENT_UNDER_TEST = new ComponentUnderTest()
            .addLogHandler(IN_MEMORY_LOG_HANDLER.getHandler())
            // A async job executor is required to process service task
            .setParameter(
                    new QName(ActivitiSEConstants.NAMESPACE_COMP, ActivitiSEConstants.ENGINE_ENABLE_JOB_EXECUTOR),
                    Boolean.TRUE.toString())
            .setParameter(
                    new QName(ActivitiSEConstants.NAMESPACE_COMP, ActivitiSEConstants.ENGINE_IDENTITY_SERVICE_CFG_FILE),
                    // Generate identity service configuration files
                    new ParameterGenerator() {

                        @Override
                        public String generate() throws Exception {
                            final URL identityServiceCfg = Thread.currentThread().getContextClassLoader()
                                    .getResource("su/valid/identityService.properties");
                            assertNotNull("Identity service config file is missing !", identityServiceCfg);
                            return new File(identityServiceCfg.toURI()).getAbsolutePath();
                        }

                    })
            .registerServiceToDeploy(VALID_SU, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/valid/vacationRequest.wsdl");
                    assertNotNull("WSDl not found", wsdlUrl);
                    final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                            VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT, wsdlUrl);

                    final URL demanderCongesResponseXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/valid/demanderCongesResponse.xsl");
                    assertNotNull("Output XSL 'demanderCongesResponse.xsl' not found", demanderCongesResponseXslUrl);
                    serviceConfiguration.addResource(demanderCongesResponseXslUrl);

                    final URL validerDemandeResponseXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/valid/validerDemandeResponse.xsl");
                    assertNotNull("Output XSL 'validerDemandeResponse.xsl' not found", validerDemandeResponseXslUrl);
                    serviceConfiguration.addResource(validerDemandeResponseXslUrl);

                    final URL ajusterDemandeResponseXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/valid/ajusterDemandeResponse.xsl");
                    assertNotNull("Output XSL 'ajusterDemandeResponse.xsl' not found", ajusterDemandeResponseXslUrl);
                    serviceConfiguration.addResource(ajusterDemandeResponseXslUrl);

                    final URL numeroDemandeInconnuXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/valid/numeroDemandeInconnu.xsl");
                    assertNotNull("Output XSL 'numeroDemandeInconnu.xsl' not found", numeroDemandeInconnuXslUrl);
                    serviceConfiguration.addResource(numeroDemandeInconnuXslUrl);

                    final URL demandeDejaValideeXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/valid/demandeDejaValidee.xsl");
                    assertNotNull("Output XSL 'demandeDejaValidee.xsl' not found", demandeDejaValideeXslUrl);
                    serviceConfiguration.addResource(demandeDejaValideeXslUrl);

                    final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/valid/vacationRequest.bpmn20.xml");
                    assertNotNull("BPMN file not found", bpmnUrl);
                    serviceConfiguration.addResource(bpmnUrl);

                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU, "process_file"),
                            "vacationRequest.bpmn20.xml");
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU, "version"), "1");

                    // Consume service 'archiver'
                    // TODO: The consume section seems mandatory to retrieve the consume endpoint on async exchange
                    // between Activiti and other services
                    final ConsumesServiceConfiguration consumeServiceConfiguration = new ConsumesServiceConfiguration(
                            ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT);
                    serviceConfiguration.addServiceConfigurationDependency(consumeServiceConfiguration);

                    return serviceConfiguration;
                }
            }).registerNativeServiceToDeploy(NATIVE_TASKS_SVC_CFG, new NativeServiceConfigurationFactory() {

                @Override
                public ServiceConfiguration create(final String nativeEndpointName) {

                    final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("component.wsdl");
                    assertNotNull("Integration servce WSDl not found", nativeServiceWsdlUrl);
                    return new ProvidesServiceConfiguration(ITG_TASK_PORT_TYPE, ITG_TASK_SERVICE, nativeEndpointName,
                            nativeServiceWsdlUrl);
                }

                @Override
                public QName getNativeService() {
                    return ITG_TASK_SERVICE;
                }
            }).registerNativeServiceToDeploy(NATIVE_PROCESSINSTANCES_SVC_CFG, new NativeServiceConfigurationFactory() {

                @Override
                public ServiceConfiguration create(final String nativeEndpointName) {

                    final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("component.wsdl");
                    assertNotNull("Integration servce WSDl not found", nativeServiceWsdlUrl);
                    return new ProvidesServiceConfiguration(ITG_PROCESSINSTANCES_PORT_TYPE,
                            ITG_PROCESSINSTANCES_SERVICE, nativeEndpointName, nativeServiceWsdlUrl);
                }

                @Override
                public QName getNativeService() {
                    return ITG_PROCESSINSTANCES_SERVICE;
                }
            }).registerExternalServiceProvider(ARCHIVE_ENDPOINT, ARCHIVE_SERVICE, ARCHIVE_INTERFACE);

    @ClassRule
    public static final TestRule chain = RuleChain.outerRule(TEMP_FOLDER).around(IN_MEMORY_LOG_HANDLER)
            .around(COMPONENT_UNDER_TEST);

    protected static final SimpleComponent COMPONENT = new SimpleComponent(COMPONENT_UNDER_TEST);

    @Override
    protected ComponentUnderTest getComponentUnderTest() {
        return COMPONENT_UNDER_TEST;
    }
}
