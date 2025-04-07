/**
 * Copyright (c) 2017-2025 Linagora
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
package org.ow2.petals.flowable.rest.config;

import org.flowable.rest.security.BasicAuthenticationProvider;
import org.ow2.petals.flowable.FlowableSE;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.CsrfConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;

/**
 * This defines the REST API security by configuring the user privilege (from the Flowable engine identity service) that
 * can access the API. Inspired by flowable-rest-app code (org.flowable.rest.conf.SecurityConfiguration).
 * 
 * @author Victor NOEL - Linagora
 * @author Jordy CABANNES - Linagora
 */
@Configuration(proxyBeanMethods = false)
@EnableWebSecurity
public class SecurityConfiguration {

    public static final String FLOWABLE_REST_API_ACCESS_PRIVILEGE_QUALIFIER = "org.ow2.petals.flowable.rest.RestApiAccessPrivilege";

    /**
     * This is injected by the SE (see {@link FlowableSE#createRestApi()}).
     */
    @Autowired
    @Qualifier(FLOWABLE_REST_API_ACCESS_PRIVILEGE_QUALIFIER)
    private String accessPrivilege;

    @Bean
    public AuthenticationProvider authenticationProvider() {
        final BasicAuthenticationProvider authProvider = new BasicAuthenticationProvider();
        authProvider.setVerifyRestApiPrivilege(true);
        return authProvider;
    }

    @Bean
    public SecurityFilterChain restApiSecurity(final HttpSecurity http,
            final AuthenticationProvider authenticationProvider)
            throws Exception {
        final HttpSecurity httpSecurity = http.authenticationProvider(authenticationProvider)
                .sessionManagement(
                        sessionManagement -> sessionManagement.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
                .csrf(CsrfConfigurer::disable);

        httpSecurity.authorizeHttpRequests(authorizeRequests -> authorizeRequests.anyRequest()
                .hasAuthority(this.accessPrivilege));

        httpSecurity.httpBasic(Customizer.withDefaults());

        return http.build();
    }

}
